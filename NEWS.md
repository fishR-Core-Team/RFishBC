# RFishBC 0.0.2 ongoing
* `digitizeRadii()`: Modified. Added `snap2Transect=` to address [#1](https://github.com/droglenc/RFishBC/issues/1). Modified how `fname=` was handled if missing (see `iHndlfname()`) and if selecting an image from outside of the current working directory. Allowed user to choose `id=` through a dialog box or a console prompt (see `iHndlID()`), which addresses [#2](https://github.com/droglenc/RFishBC/issues/2). Fixed poor directions about the use of the escape key to terminate `locator()`.
* `iHndlfname()`: Added. Used in `digitizeRadii()`, `showDigitizedImage()`, and `findScalingFactor()`.
* `iHndlID()`: Added. Used in `digitizeRadii()`.
* `iHndlScalingFactor()`: Modified. Added `pixW2H=` argument to receive the pixel width-to-height ratio to adjust distance calculations for non-square images.
* `iReadImg()`: Modified. Calculated the pixel width-to-height ratio (from image dimensions) and returned in list.
* `iSnap2Transect()`: Added. Used in `digitizeRadii()`.
* `iProcessAnnuli()`: Modified. Added `pixW2H=` argument to receive the pixel width-to-height ratio to adjust distance calculations for non-square images.
* `showDigitizedData()`: Modified. Moved `file.choose()` for `fname=` out of argument list and into the main function code. Also modified to allow the user to select an object out of the current working directory (only works with a single file).

# RFishBC 0.0.1 16-Apr-18
* First version. Everything is new.
