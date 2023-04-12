## [2.4.1] - 2022.10.19
### New
Implemented by **Jerome Spielmann** for FINMA:
  - New unit tests for the market, participation and captive non-life risk simulation methods.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:
  - Fix two typos in the FDS field names.
  - Disable internal configuration files which are maintained separately (only relevant for FINMA).

### Implemented by **Jerome Spielmann** for FINMA:
  - Refactoring of some unit tests related to simulation, reordering and processing.
  - Clean-up of some functions related to reordering and participation.
  - Removed Yes / No switch for old credit risk model and adapted related unit tests.
  - Removed outdated "Advanced Guide".

## [2.4.0] - 2021.10.11
### New
Implemented by **Laurent Dudok de Wit** for FINMA:
- Add support for using an external configuration file for the FDS (only relevant for FINMA).
- Implemented unit tests for exchange rates.

Implemented by **Jerome Spielmann** for FINMA:
- Simplification of the initial exchange rate table. Redundant inputs are not required anymore.
- Added FDS reporting for the non-life risk without scenarios.
- Add the possibility to load a SST 2021 template (with a warning message).

### Fixed
Implemented by **Jerome Spielmann** for FINMA:
  - Fixed the encoding to `UTF-8` to read configuration inputs (only relevant for FINMA).
  
### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:
- Minor syntax changes.

## [2.2.0] - 2021.05.31
### New
Implemented by **Laurent Dudok de Wit** for FINMA:
- Implemented a scenario aggregation within the non-life model for the captive fieldtest.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:
- The news file is displayed in a user-friendly way in RStudio with the function `sstNews` (this only works with R >= 4.0.0)

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:
- Fixed the check ensuring that the scenario probability are the same for the mother and daughter company. The check was not enabled for health scenarios.

## [2.1.2] - 2021.05.19
### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:
- The compatibility checks are disabled when an external configuration file is used. (only relevant for FINMA)
- An error message is shown when an old template is provided.

## [2.1.1] - 2021.05.17

### New
Implemented by **Laurent Dudok de Wit** for FINMA:
- Improved approach for the configuration of the FDS and of the parser. (only relevant for FINMA)
- Added a very short vignette intended for advanced users. (only relevant for FINMA)

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:
- Changed the function to display the news.
- Changed the export and load of the cache files. (only relevant for FINMA)
- Update the compatibility checks.

## [2.1.0] - 2021.03.11

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Allow "0" and "1" values instead of "No" and "Yes" in the SST-Template
- Improved approach to load the SST Template
- The size of the correlation matrix is automatically identified
- Multiple external configuration files can be loaded (only relevant for FINMA)
- New check to ensure that the last probability of the cumulative distribution function lies between 99.9% and 100% (only relevant for FINMA)

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:
- Improved stability of input validation when a different configuration file is used
- Improved the functionality for caching (only relevant for FINMA)

## [2.0.3] - 2020.10.16

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added a style for italic fields in the FDS.
- Added a warning in case a daughter company provides a non-zero CY risk, PY risk or `Additional effects on the TC` (only relevant for companies using the participation model)
- Added unit tests

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Add a warning message if the SST 2020 Template (Feldtest version) is used for the official solvency calculation with this R-Tool.
- Add a warning message if non-zero CY or PY risks are provided for a company that simulates the captive insurance risk with the R-Tool.
- Improved filtering to allow to invert the filter selection
- Improved filtering to allow to filter on multiple columns simultaneously
- Improved configuration file for versioning the R-Tool
- Updated the configuration file for the generation of the FDS for the SST 2021 (no impact, only relevant for FINMA)
- Improved the handling of an external configuration file (no impact, only relevant for FINMA)
- Update the vignette

Implemented by **Daniel Kauth** and **Laurent Dudok de Wit** for FINMA:

- The CY and PY risk (computed by the R-Tool) for companies whose non-life risk is simulated using the captive model is now exported in the FDS. (only relevant for captives whose insurance risk is simulated by the R-Tool)

## [2.0.2] - 2020-06-29

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added unit test for the reordering of the participation model with two daughter companies.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Fixed bug preventing from running the participation model with two daughters.

## [2.0.1] - 2020-06-23

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- No cashflows need to be provided when no migration risk is modelled.
- Improved the error message when negative cashflows are provided for the credit risk model, with a reference to the documentation.


## [2.0.0] - 2020-06-23

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Updating version number to 2.0.0 for the publication together with the SST-Tool.

## [1.3.8] - 2020-06-22
### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- The R-Tool should be used with the new SST-Template published with the feldtest. The use of an old SST-Template provides an error message and asks the user to use the currenlty official R-Tool.

## [1.3.7] - 2020-06-19
### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added comments in the R code
- Dependency `data.table` license (GPL-3 or MPL 2.0) is shown depending on whether the package is started from R or started from the executable version.
- Added unit tests for the validation of parameters for the participation model.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Fixed incorrect progress bar when multiple variants are calculated using the old credit risk model.
- Fixed missing scaling of the non-life parameter `mu` for the daughter companies and for the health scenarios (affects only the participation model).
- Improved error handling for the participation model to ensure that only the required parameters are provided. For example, it is not possible anymore to perform a standalone SST-calculation with some inputs for the participation model.
- Fixed small typos (incomplete argument matching) with no impact on the R-Tool.
- Fixed incorrect console output of the objects produced by `excelToModelSST` since the print function was not exported.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Add an error message when the captive model is used jointly with the participation model.
- Add additional unit tests for the credit risk model.

## [1.3.6] - 2020-05-31
### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Speed improvement for the generation of the FDS.
- Added unit tests for captives simulation.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Renamed some R files.
- Deleted old code.
- The scaling factor for the captive model (QS) and for the credit risk model are allowed to be equal to 100%.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Fixed incorrect captive calculation when CY risk is deterministic or CY risk is zero.
- Fixed incorrect error trigger preventing from perfoming a calculation with PY-risk only for captives.

## [1.3.5] - 2020-05-27

### New
Implemented by **Matteo Gambara** for FINMA:

- Added some unit tests for captive (non-life) companies.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added some unit tests for Health
- Added the namespace when required and added necessary names to the list of global variables.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Fixed library loading within unit tests.

Implemented by **Matteo Gambara** for FINMA:

- Fixed some old unit tests to enable compatibility with newer version.

## [1.3.4] - 2020-05-20

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added a warning when the implied spread for the credit risk model is very low or very high.
- Added an additional column `simulation_id` to the risk factors simulations. This is the same simulation id as the standalone simulations.
- Added a warning message for incorrect captive quota-share input.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Improved display of elements of class `sstModel` and `sstCalculationVariant`.
- Changed the column order of the object `Standalones_Simulations` to have `deltaRBC` and `deltaRBC_beforeLLPO` in the right position.
- Create a copy of captive portfolio to prevent any modification by reference.
- Rename the field for the `Input SST Template` in Shiny.
- Update year for the license.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Fixed warning message when missing LGD scaling.
- Fixed name of market risk factors.
- Fixed typo for spread check for fixed income instruments.
- Fixed bug for checking non-empty numerical values.
- Fixed incorrect yes/no indicator for information about the use of the new credit risk model.
- Added icons for the executable version.
- Small fix for results shown in target capital decomposition to have 0 instead of small values in order of magnitude of 1e-14 due to the computer numerical precision.

## [1.3.3] - 2020-05-12

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- The parser loads all worksheets containing values to extract. For more stability, only the relevant columns are loaded. This prevents from loading a huge data set with more than 16'000 columns if data is (inadvertently) entered in the last column.
- Added a consistency check for the LGD values.
- Added an error message when the spread from credit risk cashflows could not be computed.
- Added an error message when credit risk cashflows are missing.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Check to ensure that the Basel III classes provided in the credit risk portfolio are associated to a LGD value.
- Modified the check to give also an error when the sum of probability of the scenarios is equal to 100%.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Fixed the `Nachsilbe zum FDS` which has been disabled.
- Added a check to give an error when the risk factors (health and life) are not in the same order as in the same correlation matrix.

## [1.3.2] - 2020-04-02

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Implementation of the captive model.
- The frequency for normal and large captive claims needs to be lower than a given threshold, otherwise an error message is thrown. This is in line with the captive model principle. 
- The simulation of the non-life model is split into multiple modules.
- The non-life risk is split between CY and PY risk for captives. For other non-life risk models no distinction is made between CY and PY simulations, the simulations are therefore on purpose set to 0. In all cases, the expected shortfall of PY and CY risk is an input of the SST-Template
- The `Yes` and `No` values from the SST-Template are now converted into logical values in the `Objects` loaded.
- Added a check to ensure that the mother and daughter template are using the same version of the configuration file.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- The function to open the fundamental data sheet in Excel is now one of the element of the list produced by `simulate.sstCalculationVariant` instead of an element of the list from `simulate.sstCalculation`.
- Improved generation of the FDS, which is now generated without any Excel formula. The FDS is generated from

    + Text fields (static value)
    + Numeric values from the R-Tool output (value from a keyword)
    + Numeric values from any worksheet from the Excel template (linked value)
    
- For the FDS generation, when a linked value refers to an empty cell, 0 is returned. This gives the same behavior as Excel. Furthermore values are added when two values are added to the same cell in the FDS.
- Variants identified in an SST-Tempalte are sorted by alphabetic order.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- An error message is sent when the foreign currency for the FX forwards is equal to the SST-currency.

## [1.3.1] - 2020-03-20
### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- User-friendly display of the elements `Modules` and `Objects$Template` from a SST model.
- The date and time of generation of the configuration sheet are exported in the configuration sheet. This makes it possible to see which version is used and to ensure that the mother and daughter company use the same version.
- Most of the parameters used to generate the configuration sheet are now included in the package as a global variable.
- Show a progress bar for the credit risk model

## [1.3.0] - 2020-03-17
### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- The simulation results now show extended information. In particular the expected financial and technical results are shown (as constants) in the simulation results
- The whole FDS is produced by `sstCalculation`. No template for the FDS is anymore required.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Many code improvement and inline documentation of functions

Implemented by **Matteo Gambara**:

- Implementation of unit tests to ensure the stability of the R-Tool.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Whenever the participation model was used and the daughter risks were displayed as participation risk by the mother company, then the market risk component used for the MVM calculation (nhmr) was impacted. This is now fixed, therefore the MVM is not impacted anymore when the risk from the daughter company is displayed as a participation risk.

## [1.2.0] - 2020-02-20
### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Implementation and seamless integration of the credit risk model (initially implemented by **Matteo Gambara**) in the `sstCalculation` package.
- Object from `excelToModelSST` are now displayed in an improved way:

    + The calculation variant is shown
    + The company name is shown
    + An indicator shows whether the company is a mother company or a daughter company

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Error messages now show the name of the company:

    + The company name stems from the corresponding entry in the SST-Template
    + This is in particular relevant for the participation model to distinguish between an error from the mother company and the daughter company.

- Immaterial participations are now integrated in the market risk module.
- The R code is split across multiple file with a clearer structure


## [1.1.0] - 2020-02-05
### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- A batch mode is introduced to perform multiple model run from one SST-Template. This is an experimental feature, which is not yet intended to be used for solvency calculation purposes. It will be communicated once this feature is final.
- The batch mode works as follows:
  
    + There is one SST-Template, where some sheets are duplicated. Duplicated sheets have a name of the form `SheetName__V1`, `SheetName__V2` etc.
    + The original sheet name is called `Base case` and the duplicated sheets are variants. Here we would have a base case together with `Variant V1` and `Variant V2`.

## [1.0.0] - 2020-01-27
### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Updated news to create the version 1.0.0.

## [0.6.1] - 2020-01-27
### Changed
Implemented by **Laurent Dudok de Wit**, **Matteo Gambara** and **Johannes Meuser** for FINMA:

- Updated the vignette containing the introduction to the R-package.

## [0.6.0] - 2020-01-10
### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added the functions `sstNews` to open the vignette listing all news from the R package.
- The vignette is totally rewritten to make it clearer for the user. Use the function `sstIntroduction` to open the vignette.
- Added a short description in a comment before each function.
- The `sstCalculation` package now allows look-through calculation with multiple daughter companies. This induces following new functionalities: 

    + The type of the template (daughter or mother) is identified automatically. In short, a template is a daughter template if and only if a participation value is provided 
    + The user can provide a multiples of templates for the solvency calculation. In the Shiny dashboard, this is done by selecting one or more Excel file. In RStudio, it is done by providing a vector of paths.
    + The threshold used for flooring daughter template simulations is available in `Standalones_Simulations` in the column `flooringThreshold`. This flooring value is set to `NA` for the mother template and for any daughter template for which no LLPO is applied.
    
- The SST-Template now contains an additional `Configuration` sheet, containing all information required to process the Template and to produce the FDS.
- The total risk from the daughter company can be displayed under the `participation` risk factor, if the corresponding switch is enabled in the Excel template.
- The new health model is now implemented.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- The calculation of the expected shortfall has been improved to handle also degenerate cases properly.  

Implemented by **Matteo Gambara** for FINMA:

- Updated the vignette describing the package.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- The error handling system has been improved to make it more robust
- The expected financial result is now computed in a consistent way. The gamma factor from each daughter is used to scale the expected financial result from the corresponding daughter.
- The dependency structure between two non-life companies is now based on an independence assumption
- The threshold used for flooring daughter template simulations is available in `Standalones_Simulations` in the column `flooringThreshold`. This flooring value is set to `NA` for the mother template and for any daughter template for which no LLPO is applied.
- The code structure was re-designed to make it easier to be unit-tested.
- The sheet with the tool output is not produced, as most outputs can be obtained more transparently in the following way:
    
    + The number of simulations and seed can be specified in the SST-Template
    + Parameter (for example the MVM nhmr factor) are available in the `General Parameters` sheet for information purposes.
    + We recommend to specify a suffix in the `Intro` sheet of the SST-Template. The produced FDS will have this suffix, which enables to associate a FDS to a SST-Template.

## [0.5.2] - 2019-05-13
### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- The effect from `Weitere Marktrisiken 1` and `Weitere Marktrisiken 2` was missing in the waterfall chart, this is now fixed.
- Updated vignette with information about how to modify the internal configuration file
- Dependency `data.table` change of license in new version from GPL-3 to MPL 2.0. 

Implemented by **Matteo Gambara** for FINMA:

- Updated the vignette describing the package.

## [0.5.1] - 2019-05-11
### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added a help function `sstIntroduction` to open the vignette.
- The user can specify in the SST Template (Sheet `Intro`, cell E21) a suffix that will be be appended to the files produced by the `sstDashboard`. The suffix can be empty or any character string, for example:

    + The date of the last modification of the SST template by the company
    + The type of calculation performed, for example `_runWithLLPO`

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Replaced density plots by Waterfall charts using the same fields as the ones provided in FINMA's SST Survey 2018.
- Improved readability of the market risk instrument formulas, where `a + -b` is replaced by `a -b`
- Updated news, copyright and authorship information.
- Updated vignette with additional information about the R package
- Updated unit tests to provide no warning on newer test_that package versions.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Corrected the calculation of the `SCR`, to include the additional effects on the target capital instead of including them in the target capital only. This has no impact on the computation of the target capital and of the SST ratio.

## [0.5.0] - 2019-05-09

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- The credit risk mitigation effect from the surplus fund is applied using the credit risk mitigation factor. (only relevant for the look-through test calculation)
- A warning is shown if the LLPO is activated for a run without daughter company.
- When starting the SST Dashboard, the working directory that will be used to export the results is shown in the console.
- Added a button in the dashboard to download density plots of each standalone category.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- The indicator whether the LLPO should be applied is now set directly in the SST Template instead of using the parameter `withLLPO`. (only relevant for the look-through test calculation)

### Fix
Implemented by **Laurent Dudok de Wit** for FINMA:

- The progress bar did not complete to 1 when the mother or the daughter company had no market risk, this is now fixed.
- Providing a relative path as an argument to `sstDashboard` did not work properly, this is now fixed.
- Added error handling when the path provided to `sstDashboard` is NULL and NA.

## [0.4.1] - 2019-05-08

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- The limited liability put option (LLPO) is implemented in the context of the test calculation. The LLPO can be activated using the parameter `withLLPO` from `sstCalculation`.
- When activated, the LLPO is applied in the following way to the daughter simulations:
  
    + The simulations from market + insurance + scenario risk (column `insurance_market_scenario.all`) of the standalone simulations matrix are floored.
    + The impact from the flooring is taken into account as a scenario with positive impact under `scenario.all`
- For the test calculation, the impact from the LLPO (whenever it is activated) is reported together with the impact of the scenarios in the FDS.
- For simulations performed with a mother and daughter company, the standalone simulation vector contains an additional column `negativeDaughterValue` indicating for each `simulationId` whether the simulation was floored
- The value used for flooring the (centered) simulations is described in the technical document for the test calculation. Furthermore:

    + The value is typically negative. A warning is shown if this value is positive
    + The value is exported in the Excel file produced by the tool
- Added additional outputs to the `Run results` worksheet

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Spreads lower than -10% and higher than 30% don't trigger anymore an error but solely a warning.

Implemented by **Matteo Gambara** for FINMA:

- Updated the vignette describing the package.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Negative values for `mu` parameter for the log-normal distribution should be allowed.
- Standalone `Participation` was erroneously a valid entry as a standalone type. This standalone is only used for immaterial participation that are comonotonically aggregated to the market risk.
- Fix missing scaling factor for the life risk calculation.

## [0.4.0] - 2019-04-25

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Improved user experience with the dashboard regarding the export functionality when started from the R console:
  
    + The user can specify the directory where the output results will be downloaded, if no directory is specified, the results will be downloaded to the user directory (`My documents` on Windows systems).
    + An error is thrown if the dashboard is launched when the specified output directory does not exist.
- The resulting simulations are sorted in such a way that the sort order of the market risk simulations remains unchanged. This makes it easier for the user to compare market risk simulations from two model runs with the same seed and same number of simulations. Note that this is purely a change in the output representation and does not correspond to a model change.
- The simulation results now contain the risk factor simulations of the `life` and `health` risk factors in addition to the previously available `market` risk factor simulations.
- The standalone simulations and the risk factors simulations contain a column `simulationId` to uniquely associate each risk factor simulation to a standalone simulation.
- This change is only relevant in case the lookthrough approach is used. The standalone simulation output now contains the simulation results of the mother and of the daughter company in addition to the consolidated simulations. Thus, for a run of 1'000'000 simulations, it consists of a table with 3'000'000 entries. The additional column `simulationType` contains the following label for each set of simulation:
   
    + `SST Calculation` for the consolidated simulations (note that currenty no LLPO is applied)
    + `Mother only` for the simulations of the mother
    + `Daughter only` for the simulations of the daughter

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Consistently with the `SST Dashboard` application, the dashboard is now launched from the R console using the `sstDashboard` function.
- A consistent naming convention is used for the simulation results:
  
    + The `Standalones_Simulations` output contains the simulations of all standalone components constituting the one-year risk capital.
    + The `Risk_factors_simulations` output contains the risk factors simulations.
- Whenever a warning message is shown in the SST dashboard, the user can click anywhere in the window to close the warning. It is not anymore needed to click on the `Ok` button.
- Re-design of the functions used to simulate a `sstCalculation` object to make them easier to be unit-tested.
- Removed the hard-coded list of risk types, which is now contained in the internal configuration file.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Whenever an error message and a warning message were thrown in the same model run without reference to a specific cell, both appeared together as a `warning`.
- When a file is downloaded from the SST Dashboard, the path is displayed correctly. Previously, the path used to contain both `\\` and `/`

## [0.3.3] - 2019-04-25

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added unit tests `process` and `simulate` functions.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Remove unused private functions.
- Replace empty cells by blank cells in the Excel Template `Fundamental_Data.xlsx`
- Change the arguments of the functions used for the generation of the risks (non-life, market, participation, scenario and reordering) to make them easier to be unit-tested.
- New function to get the seed for each risk type, the results remains unchanged
- Change in the loader to avoid an undesired warning which occurred while loading the SST template, when a recent version of `tibble` is used.
- The Dashboard is now named `SST Dashboard`

Implemented by **Matteo Gambara** for FINMA:

- Updated the vignette describing the package.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Fixed income with negative market value instruments with non-empty spread always warned that the spread was incorrect
- Minor change to the processing of delta sensitivities, where the financial instrument was modified by reference.
- Fixed shiny dashboard crash whenever the target capital is zero, since the SST ratio is not well defined.
- Fixed initial number of simulations shown in the shiny dashboard to 1000000.


## [0.3.2] - 2019-04-17

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added unit tests for the `check`, `transform` functions.
- Added some unit tests for the `functions`, `get` and `process` functions.

Implemented by **Matteo Gambara** for FINMA:

- Added a vignette describing the use of the `sstCalculation` package.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- All life and health risk are simulated, instead of simulating only the risk factors having a non-zero sensitivity. This allows the consistency with the market risk simulation technique. This enables the modelling of a look-through for the case where both the daughter and the mother have life (or health) risk.
- Change the arguments of the functions used for processing financial instruments to make them easier to be unit-tested.
- Remove unused private functions.
- Update copyright year.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Minor change to the processing of financial instruments, where the name was added twice.
- Minor change to the processing of financial instruments, where the financial instrument was modified by reference.

Implemented by **Johannes Meuser** for FINMA:
- Minor corrections to documentation and news file

## [0.3.1] - 2019-04-10

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- A warning is shown if the daughter and mother company have non-life risk. If this is the case, the non-life risk of the daughter and mother will be aggregated comonotonically.
- An error is thrown if the daughter and mother template don't have the same market risk, life risk health risk and aggregation inputs.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- Renaming the R scripts and grouping functions in a more meaningful way.
- Removed unused worksheets from the `TemplateFDS.xlsx` as the configuration of the tool is contained in the internal data. 

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- In the generation of the error message, an integer was converted to a double when adding 1. Now using the integer variant `1L`.
- The error message refers multiple times to the same cell if the cell belongs to multiple tables. The error message now refers at most once to a cell. 

## [0.3.0] - 2019-04-10

### New
Implemented by **Laurent Dudok de Wit** for FINMA:

- Added the method `excelToModelSST` to parse the SST template into a model, which can be then simulated using the generic method `simulate`.
- Added documentation entries for all exported functions.
- Added an error in the scaling factor applied to the daughter company is zero.
- Added a warning message to indicate that the default value is used for the non hedgeable market risk factor in the case no best-estimates of liability is provided.
- Added a warning in the case any best-estimate of liability is negative.
- Added an error handling for the case the risk factor `type` is missing in the risk factor mapping table.
- Improved methodology to export the FDS almost instantaneously.

### Changed
Implemented by **Laurent Dudok de Wit** for FINMA:

- The coordinates and attributes of all tables and values to be read from the SST template are now specified in an internal data file. This provided more transparency compared to the former implementation where these information were in an Excel file, as raw data. The code used to generate the data set from the SST template information is also included.
- The number of market risk factors is automatically determined based on the number of columns of the correlation matrix. The standard model uses 39 risk factors and up to 100 risk factors can be identified with this method.
- For negative insurance liability cash-flow, the warning message now additionally states that a negative cash-flows correspond to a gain.
- Improved the performance for parsing the SST template. Once all relevant sheets are loaded, the model is almost instantaneously created.
- The step `Validating` is not shown anymore as it is almost instantaneous.
- If the number of simulations is not provided in the SST template, it is set to 1000000
- Improved error message related to the risk factor mapping table to refer to the risk factor `type` affected by the error.

### Fixed
Implemented by **Laurent Dudok de Wit** for FINMA:

- The error message thrown for missing macro economic risk factors did not refer to the macro economic table.
- The error messages were displayed on the same line as the progress bar.

## [0.2.3] - 2019-04-05

First development release implemented for FINMA by **Laurent Dudok de Wit** with the support of **Matteo Gambara** for changes made to the shiny app. This release uses parts of the code from the shiny app and its launcher originally implemented by **Adrien Lamit** (@alamit) for FINMA in the R package `sstModel` version 1.0.0.

### General

Tool features

- This version allows computations of market risk, life, health and non-life insurance risks, their aggregation using the reordering algorithm with a Gaussian copula and the computation of solvency figures based on those computations.
- A look-through method is implemented in the context of a test calculation.
- Major improvement for loading and checking the SST template and for simulating market risks.
- The market risk exposure is accessible within one single `instruments` data.table.
- The reordering of total market, life, non-life and health insurance risk is propagated to RBC standalone simulations, which are stored in the `RBC_simulations` data.table. The same holds for market risk factor simulations, which are stored in the  `Market_risk_factors_simulations` data.table.
- An error handling mechanism is now used, where all errors are thrown using a specific condition class `errorHandling`.
- A progress bar now indicates the progress while loading the SST template and while the simulations are performed.

The author thanks **Matteo Gambara** for this contribution to writing some unit tests and **Johannes Meuser** for installing some releases and seeing whether the functionalities are, from the perspective of a user, understandable and working as expected.

