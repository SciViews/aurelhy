# aurelhy 1.0.9

-   Correction of a message (y0 instead of x0).

# aurelhy 1.0.8

-   Code adaptation to make it compatible with R v 4.0.0.

-   Internal rearrangement of the code (code linting).

# aurelhy 1.0-7

-   Title set in title case. Complete sentence in description, and other notes that complain in R CMD check with R 3.2.1.

-   Recompression of the example datasets (`R CMD build --resave-data`).

-   Added svUnit support for test units.

# aurelhy 1.0-6

-   Calculation of central sector in case of rectangular grid was incorect in the `aurelhy()` function (bug report by Pierre Lassegues). Corrected.

-   Transformation of latitude/longitude in decimal degrees assumed one degree equals 110.9km. This is not true (even close to) everywhere on earth! New functions `deg.lat()` and `deg.lon()` now calculate this distance according to latitude according to the WGS84 ellipsoid model. All function depending on the conversion from latitude/longitude degrees into km now use these calculations. However, for the sake of simplicity of the algorithms, a single conversion factor matching the latitude at the center of the geographic area under consideration is used. Functions affected by this change are: `aurelhy()`, `auremask()` and `polar.coords()`.

# aurelhy 1.0-5

-   Check of argument passed through ... in `update()` and issue an error message.

A new dependence to package **sp** is added. Dependence to **shapefiles** package becomes a suggestion (it is not strictly required for the execution of the AURELHY interpolation).

# aurelhy 1.0-4

-   Added a function `dist2sea()` by Francois Delobel.

-   More flexibility in the size of `add.vars`: can be on same grid as 'geotm', or same grid as the final landscape descriptors.

-   To avoid confusion between the mask used to determine which points to consider and the mask used to select sectors for landscape predictors calculation, the latter one is renamed "window of analysis".

-   `mbord` and `mmask` data are not strictly Morocco territory, but a landscape around Morocco that is the target of the example analysis. Corresponding help pages are modified accordingly.

# aurelhy 1.0-3

-   A bug in `predict.aurelhy()` (function `checkCorner()`) sometimes prevented correct selection of the closest corner in the DEM when a station is not exactly on a node, but very close to it (thanks Francois Delobel for the patch).

-   A new argument in `aurelhy()`, `resample.geomask=` indicates if the 'geomask' object should be resampled (provided with same grid as the 'geomat' object), or not (provided with same grid as final interpolation).

-   `predict.aurelhy()` can now accept argument `v.fit = FALSE`. In this case, residuals are not kriged. This saves computation when kriging is not required, e.g., when one look for best predictors in the regression. All methods appled to 'predict.aurelhy' objects are adapted to cope with objects that do not have kriged residuals recorded.

-   `add.points()` generic function added, and method for 'geomask': adds points that are outside of a 'geomask', but inside of the grid from a 'geopoints' object. It allows to add stations that are close to, but outside the area of interest (useful for correct kriging of the residuals).

# aurelhy 1.0-2

-   A warning about coercion of double to logical in `predict.aurelhy()` is solved (the coercion was intended, but as it was implicit, R generated the warning).

# aurelhy 1.0-1

-   If a 'geoshapes' object had no "shapes" attribute (like the `mbord` example dataset), `write.geoshapes()` failed. Now the "shapes" data are reconstructed from the object itself, if missing.

-   `geopoints()` incorrectly returned a 'geospoints' object. Corrected.

-   `read.geomask()` returned an object of class c('geomask', 'matrix'), instead of c('geomask', 'geomat', 'matrix'). Corrected.

-   `read.geomask()` lost the 'coords' attribute. Corrected.

-   `aurelhy()` had sometimes a problem of recursive argument because of `auremask = auremask()`. The argument is renamed `landmask. = auremask()`; same for `vgm = vgm()` renamed `vgmodel = vgm()`. Idem in `update()` method. Finally, the regression model is now provided as a string instead than as a formula.

# aurelhy 1.0-0

-   This is the first complete version with all functions implemented!

-   The `update()` method of 'aurelhy' object is now implemented, and the example is expanded to show how one can use it to explore various interpolation parameters easily.

-   `plot(x, which = 7)` graph for 'aurely.predict' objects is now implemented.

-   **aurelhy** can now also handle a rectangular grid ('auremask' object).

-   `print()` and `plot()` methods for a rectangular grid ('auremask' object) are now implemented.

-   `auremask(type = "rectangular")` calculated a grid of n-1 rectangles, corrected.

# aurelhy 0.1-6

-   Complete AURELHY interpolation is implemented now for radial 'auremask'. There are also `print()`, `summary()` and `plot()` methods for further diagnostics.

-   Interpolated 'geomat' objects are extracted from 'aurelhy' and 'aurelhy.predict' objects using the `as.geomat()` method.

-   A new dataset, `mrain`, is added: it is rain data measured at 43 stations.

-   A complete example AURELHY interpolation is ready now in the `?aurelhy` man page.

# aurelhy 0.1-5

-   'geopoints' objects are added. They are more convenient to use than 'geoshapes' to manipulate a series of variables measured at given georeferenced points (stations). There are also `read.geopoints()` and `write.geopoints()` functions, as well as `print()` and `points()` methods.

-   An example 'geopoints' object is added: `mpet`. It is PET measurements at various weather station on Morocco's territory and around.

# aurelhy 0.1-4

-   `morocco` and `mmask` datasets are enlarged a little bit to have enough border data to calculate landscape descriptors as far as 26km away from extreme points inside the Morocco territory.

-   `mseadist` dataset is added.

-   The 'aurelhy' object is added. It is the main object to perform the AURELHY interpolation, which is done using its `predict()` method. Methods `print()`, `summary()`, `plot()`, `points()` are also available. Methods `update()` and `predict()` are not implemented yet in this version.

# aurelhy 0.1-3

-   `read.geomat()`: the file is now read silently.

-   Bug correction in `write.geomat()`: the matrix was transposed.

-   `read.geoshapes()`: the function failed when coordinates where named 'X' and 'Y' instead of 'x' and 'y'. Also adde the possibility to read the DBF file at the same time ('dbf' attribute of the object).

-   geoshapes objects now have a `dbf=` attribute that holds the table of the associated DBF file.

-   It is now possible to supply custom attributes in `plot.auremask()`.

# aurelhy 0.1-2

-   Addition of 'geoshapes' objects + read/write ESRI shapefiles and `print()`, `lines()` and `points()` methods for these objects.

-   Addition of a 'geoshapes' example object (Morocco borders in `mbord`).

-   Addition of an example 'geomask' object (Morocco territory mask in `mmask`).

-   Objects 'dem' are renamed 'geotm' for "georeferenced terrain model" for a more homogene naming in comparison with the other geo\* objects

# aurelhy 0.1-1

-   There are now three grid objects: 'geomat', 'dem' and 'geomask'.

-   More plot type as `plot()`, `contour()`, `image()` and `persp()` for 'geomat' objects.

-   An example 'dem' object (Morocco with a grid of 1km x 1km roughly) is added.

# aurelhy 0.1-0

-   This is the first version on R-forge.
