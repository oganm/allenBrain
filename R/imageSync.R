#' Acquire image id centered on the region
#' @return A named list including the ID of the image and coordinates of the brain region
#' @export
structureToImage = function(datasetID,regionIDs){
    url =  glue::glue('http://api.brain-map.org/api/v2/structure_to_image/{datasetID}.xml?structure_ids={paste(regionIDs,collapse =",")}')
    xml = RCurl::getURL(url) %>% (XML::xmlParse) %>% (XML::xmlToList)
    
    structureImage = data.frame(matrix(unlist(xml$`image-sync-helper-image-syncs`), nrow=length(xml$`image-sync-helper-image-syncs`), byrow=T),stringsAsFactors = FALSE)
    names(structureImage) = names(xml$`image-sync-helper-image-syncs`$`images-sync-helper-images-syncs`) %>% make.names
    return(structureImage)
}


#' For a list of target SectionImages, find the closest (x,y) location as defined by a seed SectionImage and seed (x,y) location.
#' @export
imageToImage2D = function(seedImage,x,y,images){
    xml = glue::glue('http://api.brain-map.org/api/v2/image_to_image_2d/{seedImage}.xml?x={x}&y={y}&section_image_ids={paste(images,collapse = ",")}')  %>% 
        (XML::xmlParse) %>% (XML::xmlToList)
    syncs =  data.frame(matrix(unlist(xml$`image-sync-helper-image-syncs`), nrow=length(xml$`image-sync-helper-image-syncs`), byrow=T),stringsAsFactors = FALSE)
    names(syncs) = names(xml$`image-sync-helper-image-syncs`$`images-sync-helper-images-syncs`) %>% make.names
    return(syncs)
}

#' For a list of target SectionDataSets, find the closest SectionImage and (x,y) location as defined by a seed SectionImage and seed (x,y) pixel location.
#' @export
imageToImage = function(seedImage,x,y,datasets){
    xml = glue::glue('http://api.brain-map.org/api/v2/image_to_image/{seedImage}.xml?x={x}&y={y}&section_data_set_ids={paste(datasets,collapse=",")}')  %>% 
        (XML::xmlParse) %>% (XML::xmlToList)
    syncs =  data.frame(matrix(unlist(xml$`image-sync-helper-image-syncs`), nrow=length(xml$`image-sync-helper-image-syncs`), byrow=T),stringsAsFactors = FALSE)
    names(syncs) = names(xml$`image-sync-helper-image-syncs`$`images-sync-helper-images-syncs`) %>% make.names
    return(syncs)
}

#' Acquire atlas id centered on the image
#' @param imageID ID of the input image
#' @param x x coordinate of the location of the desired center coordinates on the image
#' @param y y corrdinate of the location of the desired center coordinates on the image
#' @param planeOfSection sagittal or coronal atlas?
#' @return A named list including the ID of the image and coordinates closest to the coordinates in the input
#' @export
imageToAtlas = function(imageID,x,y,planeOfSection =c('sagittal','coronal')){
    planeOfSection = match.arg(planeOfSection)
    
    POS = switch (planeOfSection,
                  sagittal = 2,
                  coronal = 1)
    
    image = RCurl::getURL(glue::glue('http://api.brain-map.org/api/v2/image_to_atlas/{imageID}.xml?x={x}&y={y}&atlas_id={POS}')) %>% (XML::xmlParse) %>% (XML::xmlToList)
    out = image$`image-sync`
    names(out) = make.names(names(out))
    return(out)
    # list(imageID = image$`image-sync`$`section-image-id` %>% as.numeric,
    #   x = image$`image-sync`$x %>% as.numeric,
    #   y = image$`image-sync`$y %>% as.numeric)
}
