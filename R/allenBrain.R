#' Acquire datasets for genes
#'
#' @param gene A mouse gene symbol
#' @param planeOfSection coronal or sagittal
#'
#' @return Dataset ID of section datasets matching the description (gene and section)
#' @export
getGeneDatasets = function(gene,
                          planeOfSection = c('sagittal', 'coronal','both'),
                          probeOrientation = c('antisense','sense','both')){
    
    planeOfSection = match.arg(planeOfSection)
    probeOrientation = match.arg(probeOrientation)
    
    POS = switch (planeOfSection,
                  sagittal = 2,
                  coronal = 1,
                  both= NULL)
    orientation = switch(probeOrientation,
                         antisense = 2,
                         sense = 1,
                         both = NULL)
    
    
    sectionDataSets = RCurl::getURL(paste0('http://api.brain-map.org/api/v2/data/SectionDataSet/query.xml?criteria=products[id$eq1],genes[acronym$eq%27',
                                    gene,
                                    '%27]&include=genes,section_images,probes')) %>% (XML::xmlParse) %>% (XML::xmlToList)
    
    # loop to find the first experiment with the right plane of section
    relevant = sapply(1:length(sectionDataSets$`section-data-sets`), function(i){
        (is.null(POS) || sectionDataSets$`section-data-sets`[[i]]$`plane-of-section-id` == POS) & 
            sectionDataSets$`section-data-sets`[[i]]$failed == 'false' & 
            (is.null(orientation) || sectionDataSets$`section-data-sets`[[i]]$probes$probe$`orientation-id`$text ==orientation)
    })
    
    sectionDataSets$`section-data-sets`[relevant] %>% sapply(function(x){
        c(x$id)
    }) %>% return()
}

#' Acquire a list of structure ids
#' @return A data frame with 2 columns. All names are in lower case
#' @export
getStructureIDs = function(){
    tree = RCurl::getURL('http://api.brain-map.org/api/v2/structure_graph_download/1.json')
    tree = gsub('null','-9999',tree)
    
    abaRegions = RJSONIO::fromJSON(tree)[[6]]
    abaRegions %<>% unlist(recursive=T)
    IDs = abaRegions[abaRegions %>%
                         names %>% 
                         grepl(pattern = '(^|\\.)id',x = .)]
    IDs[IDs == -9999] = NA
    IDs %<>%  as.numeric
    names = abaRegions[abaRegions %>%
                           names %>% 
                           grepl(pattern = 'name',x = .)] %>% tolower
    
    return(data.frame(id = IDs, name = names))
}

#' Acquire image id centered on the region
#' @return A named list including the ID of the image and coordinates of the brain region
#' @export
getImageID = function(datasetID,regionID){
    image = RCurl::getURL(paste0('http://api.brain-map.org/api/v2/structure_to_image/',datasetID,'.xml?structure_ids=',regionID)) %>% (XML::xmlParse) %>% (XML::xmlToList)
    c(imageID = image$`image-sync-helper-image-syncs`$`images-sync-helper-images-syncs`$`section-image-id` %>% as.numeric,
      x = image$`image-sync-helper-image-syncs`$`images-sync-helper-images-syncs`$x %>% as.numeric,
      y = image$`image-sync-helper-image-syncs`$`images-sync-helper-images-syncs`$y %>% as.numeric)
}

#' Download the image from ABA
#' @param imageID id of an image acquired from getImageID function
#' @param output file to output the image. If null a magick-image object is returned
#' @param view what kind of image to download, expression gives grayscale processed image while projection is the raw photo
#' @param downsample downsampling rate. Unless 0, coordinates from getImageID function won't be exact
#' @export
dowloadImage = function(imageID,outputFile = NULL,view = c('expression','projection'),downsample = 0){
    
    view = match.arg(view)
    
    if(!is.null(outputFile)){
        download.file(url = paste0('http://api.brain-map.org/api/v2/image_download/',
                                   imageID,
                                   '?downsample=',
                                   downsample,
                                   '&view=',view),destfile = outputFile)
    } else {
        image = magick::image_read(paste0('http://api.brain-map.org/api/v2/image_download/',
                                          imageID,
                                          '?downsample=',
                                          downsample,
                                          '&view=',view))
        return(image)
    }
}



#' Center and crop the image
#' 
#' @param image a file path as a character or a magick-image
#' @param x center x coordinate of the image
#' @param y center y coordinate of the image
#' @param xProportions vector of length 2. If \code{c(0.1,0.1)} 20% of the image will be kept in x dimension. 10% to the left and 10% to the right
#' @param yProportions same as xProportions but for y
#' @param outputFile file path of the output. If null, a magick-image is returned
#' @export
centerImage = function(image, x ,y , xProportions = c(0.1,0.1), yProportions =c(0.1,0.1) , outputFile=NULL){
    if ('magick' %in% (installed.packages() %>% rownames())){
    if(is.character(image)){
        image = magick::image_read(image)
    }
    dimensions = magick::image_info(image) %>% {c(.$width,.$height)}
    } else{
        warning("Package 'magick' is not installed. Using system calls to imagemagick")
        dimensions = system(paste('identify',
                                  imageFile), intern = TRUE) %>%
            regmatches(.,regexpr("(?<=[ ])[0-9]*?x[0-9]*?(?=[ ])",.,perl=T)) %>%
            strsplit('x') %>% .[[1]] %>% as.double
    }

    sizeX = (dimensions[1]*(xProportions[1] + xProportions[2])) %>% round
    sizeY = (dimensions[2]*(yProportions[1] +yProportions[2])) %>% round
    
    shiftX = (dimensions[1]*(xProportions[1])) %>% round
    shiftY = (dimensions[2]*(yProportions[1])) %>% round
    
    beginningX = round(x) - shiftX
    beginningY = round(y) - shiftY
    
    if ('magick' %in% (installed.packages() %>% rownames())){
        image = magick::image_crop(image,geometry = glue::glue('{sizeX}x{sizeY}+{beginningX}+{beginningY}'))
        if (is.null(outputFile)){
            return(image)
        } else{
            magick::image_write(image,path = outputFile)
        }
    } else{
        system(paste0('convert "',imageFile, '" -crop ',sizeX,'x',sizeY,'+',beginningX,'+',beginningY,' "',outputFile,'"'))
    }
}

gridData = function(datasetID,outputFile ,include = c('energy','density','intensity')){
    include %<>% paste(collapse=',')
    link = glue::glue('http://api.brain-map.org/grid_data/download/{datasetID}&include={include}')
    download.file(link,destfile = outputFile)
}

