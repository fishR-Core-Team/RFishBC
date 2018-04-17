# RFishBC 0.0.2 ongoing
* `digitizeRadii()`: Modified. Added `snap2Transect=` to address [#1](https://github.com/droglenc/RFishBC/issues/1). Modified how `fname=` was handled if missing (see `iHndlfname()`). Allowed user to choose `id=` through a dialog box or a console prompt (see `iHndlID()`), which addresses [#2](https://github.com/droglenc/RFishBC/issues/2). Fixed poor directions about the use of the escape key to terminate `locator()`.
* `iHndlfname()`: Added. Used in `digitizeRadii()`, `showDigitizedImage()`, and `findScalingFactor()`.
* `iHndlID()`: Added. Used in `digitizeRadii()`.
* `iSnap2Transect()`: Added. Used in `digitizeRadii()`.
* `showDigitizedData()`: Modified. Moved `file.choose()` for `fname=` out of argument list and into the main function code.

# RFishBC 0.0.1 16-Apr-18
* First version. Everything is new.
