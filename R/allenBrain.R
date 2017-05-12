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






#' Downloads atlas image
#' @export
downloadAtlas = function(imageID,outputFile = NULL,downsample = 0){
    link = glue::glue('http://api.brain-map.org/api/v2/atlas_image_download/{imageID}?downsample={downsample}&annotation=true')
    
    if(!is.null(outputFile)){
        download.file(url = link,destfile = outputFile)
    } else {
        image = magick::image_read(link)
        return(image)
    }
}

#' List images for a dataset
#' @export
listImages = function(datasetID){
    xml = glue::glue('http://api.brain-map.org/api/v2/data/query.xml?criteria=model::SectionImage,rma::criteria,[data_set_id$eq{datasetID}]')  %>% (XML::xmlParse) %>% (XML::xmlToList)
    images <- data.frame(matrix(unlist(xml$`section-images`), nrow=length(xml$`section-images`), byrow=T),stringsAsFactors = FALSE)
    names(images) =names(xml$`section-images`$`section-image`) %>% make.names
    return(images)
}


#' @export
gridData = function(datasetID,outputFile ,include = c('energy','density','intensity')){
    include %<>% paste(collapse=',')
    link = glue::glue('http://api.brain-map.org/grid_data/download/{datasetID}&include={include}')
    download.file(link,destfile = outputFile)
}

