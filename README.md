# allenBrain
Acquiring pictures from allen brain atlas in R. Cropping and centering images requires [ImageMagick](http://www.imagemagick.org/).

Install ImageMagick from command line
```
sudo apt-get install imagemagick
```

Install allenBrain to R
``` r
devtools::github_install('oganm/allenBrain')
```




## Example usage
``` r
IDs = getStructureIDs()

# get the id of the desired region
granuleID = IDs[grepl('dentate gyrus, granule cell layer',IDs$name),]$id

# get the dataset for the desired gene
datasetID = getGeneDataset(gene = 'Prox1',planeOfSection = 'sagittal')

granuleID = IDs[grepl('dentate gyrus, granule cell layer',IDs$name),]$id


# get the slide that has the desired brain region and coordinates of the center of the region
imageID = getImageID(datasetID = datasetID, regionID = granuleID)

# download the slide
dowloadImage(imageID["imageID"], view = 'projection',output = 'image.jpg')

# crop the slide so that the desired brain region is in the center
centerImage(imageFile = 'image.jpg', x = imageID['x'],
            y= imageID['y'],
            xProportion = .2,
            yProportion =.2,
            outputFile = 'cropped.jpg')
```
