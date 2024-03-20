#' Center and crop the image
#' 
#' @param image a file path as a character or a magick-image
#' @param x center x coordinate of the image
#' @param y center y coordinate of the image
#' @param xProportions vector of length 2. If \code{c(0.1,0.1)} 20\% of the image will be kept in x dimension. 10\% to the left and 10\% to the right
#' @param yProportions same as xProportions but for y
#' @param outputFile file path of the output. If null, a magick-image is returned
#' @param downsample downsampling used when image was downloaded
#' @export
centerImage = function(image, x ,y , xProportions = c(0.1,0.1), yProportions =c(0.1,0.1) , outputFile=NULL,downsample = 0){
    if ('magick' %in% (installed.packages() %>% rownames())){
        if(is.character(image)){
            image = magick::image_read(image)
        }
        dimensions = magick::image_info(image) %>% {c(.$width,.$height)}
    } else{
        warning("Package 'magick' is not installed. Using system calls to imagemagick")
        dimensions = system(paste('identify',
                                  image), intern = TRUE) %>%
            regmatches(.,regexpr("(?<=[ ])[0-9]*?x[0-9]*?(?=[ ])",.,perl=T)) %>%
            strsplit('x') %>% .[[1]] %>% as.double
    }
    x = as.numeric(x)/(2^downsample)
    y = as.numeric(y)/(2^downsample)
    sizeX = (dimensions[1]*(xProportions[1] + xProportions[2])) %>% round
    sizeY = (dimensions[2]*(yProportions[1] +yProportions[2])) %>% round
    
    shiftX = (dimensions[1]*(xProportions[1])) %>% round
    shiftY = (dimensions[2]*(yProportions[1])) %>% round
    
    beginningX = round(as.numeric(x)) - shiftX
    beginningY = round(as.numeric(y)) - shiftY
    
    if ('magick' %in% (installed.packages() %>% rownames())){
        image = magick::image_crop(image,geometry = glue::glue('{sizeX}x{sizeY}+{beginningX}+{beginningY}'))
        if (!is.null(outputFile)){
            magick::image_write(image,path = outputFile)
        }
        return(image)
    } else{
        # this is actually not possible now that it is required
        warning("magick package is not available")
        system(paste0('convert "',image, '" -crop ',sizeX,'x',sizeY,'+',beginningX,'+',beginningY,' "',outputFile,'"'))
    }
}


#' Adding a scale bar to the image
#' 
#' @param image a file path as a character or a magick-image
#' @param resolution Image resolution as acquired via \code{\link{getSectionImage}}
#' @param margins Percent ratios as distance from the bottom left corner of the image
#' @param width Percent ratio of image width to use for scale width
#' @param outputFile file path of the output.
#'
#' @export
add_scale = function(image, resolution, downsample,
                     margins = c(0.1,0.1),
                     width = 0.2,
                     outputFile=NULL){
    microns_per_pixel = as.numeric(resolution)*(2^downsample)
    
    if(is.character(image)){
        image = magick::image_read(image)
    }
    dimensions = magick::image_info(image) %>% {c(.$width,.$height)}
    
    scale_width = round(dimensions[1]*width)
    scale_height = round(scale_width/20)
    
    # just a 1x1 black pixel...
    black = magick::image_read(structure(c(0, 0, 0), dim = c(1L, 1L, 3L)))
    # extend to turn into a line of appropriate size
    black = magick::image_extent(black,glue::glue('{scale_width}x{scale_height}'),color='black')
    black = magick::image_extent(black,glue::glue("{scale_width}x{scale_height*20}"),color = 'white',gravity= 'North')
    black = magick::image_extent(black,glue::glue("{scale_width+5}x{scale_height*20+5}"),color = 'white',gravity= 'Center')
    
    
    black = magick::image_annotate(black,
                                   size = scale_height*2.5,
                                   boxcolor = 'gray98',
                                   location = glue::glue("+5+{scale_height+7}"),
                                   text = glue::glue("{round(microns_per_pixel*scale_width,digits = 2)} microns"))
    black = magick::image_trim(black)
    
    back_info = magick::image_info(black)
    
    black = magick::image_extent(black,
                                 glue::glue("{dimensions[1]*margins[1]+back_info$width}x{dimensions[2]*margins[2] + back_info$height}"),
                                 gravity = "NorthEast")
    
    black = magick::image_extent(black,geometry = glue::glue("{dimensions[1]}x{dimensions[2]}"),gravity = 'SouthWest')
    
    image = magick::image_flatten(c(image,black))
    if (!is.null(outputFile)){
        magick::image_write(image,path = outputFile)
    }
    return(image)
}
