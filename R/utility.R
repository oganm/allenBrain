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
