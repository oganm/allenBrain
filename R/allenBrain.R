#' Acquire datasets for genes
#'
#' @param gene A mouse gene symbol
#' @param planeOfSection coronal or sagittal
#'
#' @return Dataset ID of section datasets matching the description (gene and section)
#' @export
getGeneDatasets = function(gene,
                          planeOfSection = c('sagittal', 'coronal','all')){
    
    planeOfSection = match.arg(planeOfSection)
    
    POS = switch (planeOfSection,
                  sagittal = 2,
                  coronal = 1,
                  all= NULL)
    
    
    sectionDataSets = getURL(paste0('http://api.brain-map.org/api/v2/data/SectionDataSet/query.xml?criteria=products[id$eq1],genes[acronym$eq%27',
                                    gene,
                                    '%27]&include=genes,section_images')) %>% (XML::xmlParse) %>% (XML::xmlToList)
    
    # loop to find the first experiment with the right plane of section
    relevant = sapply(1:length(sectionDataSets$`section-data-sets`), function(i){
        (is.null(POS) || sectionDataSets$`section-data-sets`[[i]]$`plane-of-section-id` == POS) & 
            sectionDataSets$`section-data-sets`[[i]]$failed == 'false'
    })
    
    sectionDataSets$`section-data-sets`[relevant] %>% sapply(function(x){
        c(x$id)
    }) %>% return()
}


#' Acquire image id centered on the region
#' @return A named list including the ID of the image and coordinates of the brain region
#' @export
getImageID = function(datasetID,regionID){
    image = getURL(paste0('http://api.brain-map.org/api/v2/structure_to_image/',datasetID,'.xml?structure_ids=',regionID)) %>% (XML::xmlParse) %>% (XML::xmlToList)
    c(imageID = image$`image-sync-helper-image-syncs`$`images-sync-helper-images-syncs`$`section-image-id` %>% as.numeric,
      x = image$`image-sync-helper-image-syncs`$`images-sync-helper-images-syncs`$x %>% as.numeric,
      y = image$`image-sync-helper-image-syncs`$`images-sync-helper-images-syncs`$y %>% as.numeric)
}

#' Download the image from ABA
#' @param imageID id of an image acquired from getImageID function
#' @param output file to output the image
#' @param view what kind of image to download, expression gives grayscale processed image while projection is the raw photo
#' @param downsample downsampling rate. Unless 0, coordinates from getImageID function won't be exact
#' @export
dowloadImage = function(imageID,output,view = c('expression','projection'),downsample = 0){
    
    view = match.arg(view)
    
    download.file(url = paste0('http://api.brain-map.org/api/v2/image_download/',
                         imageID,
                         '?downsample=',
                         downsample,
                         '&view=',view),destfile = output)
}



#' Center and crop the image
#' @export
centerImage = function(imageFile, x ,y , xProportions = c(0.1,0.1), yProportions =c(0.1,0.1) , outputFile){
    image = jpeg::readJPEG(imageFile, native=TRUE)
    
    

    sizeX = (dim(image)[2]*(xProportions[1] + xProportions[2])) %>% round
    sizeY = (dim(image)[1]*(yProportions[1] +yProportions[2])) %>% round
    
    shiftX = (dim(image)[2]*(xProportions[1])) %>% round
    shiftY = (dim(image)[1]*(yProportions[1])) %>% round
    
    beginningX = round(x) - shiftX
    beginningY = round(y) - shiftY
    
    system(paste0('convert "',imageFile, '" -crop ',sizeX,'x',sizeY,'+',beginningX,'+',beginningY,' "',outputFile,'"'))
    
    
    #output  = image[beginningY:(beginningY+sizeY), beginningX:(beginningX + sizeX)] 
    
    #dimOut = dim(output)
    #class(output) = 'nativeRaster'
    #attr(output, 'channels') = 3
    #attr(output, 'dim') = dimOut
    

    #jpeg::writeJPEG(image = output, target = outputFile)
}

