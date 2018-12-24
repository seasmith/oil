## Texas Rail Road Commission

### General Production Query

* Form input
* Table output
    * Data lies in `class='DataGrid'`
    * "View All Results" is at link `<a href='/PDQ/changePageViewAction.do?pagesize=500'
    * Change view type general link style `<a href='/PDQ/changeViewReportAction.do?viewType={view type here}'`
        * Monthly Totals
        * Annual Totals
        * County [x]
        * Field [x]
        * Operator [x]
            * Too many operators to display in one table
            * "Next" button `<a href='/PDQ/changePageViewAction.do?pager.offset=20'>`
              * Query string `pager.offset=520` (520 or some other offset number)
        * Lease [x]
        * District

### Disposition Codes

* http://www.rrc.state.tx.us/media/24470/pr-disposition-codes.pdf

#### CRUDE OIL/CONDENSATE DISPOSITION CODES: Column #8

Code | Definition | Description
-----|------------|------------
0    | Pipeline   |
1    | Truck      |
2    | Tank car or barge |
3    | Net oil/condensate from commercial tank cleaning | Calculated on the basis of a shakeout test. Show BS&W as oil/condensate disposition Code 6. Indicate the name of tank service and/or R-2 Facility in REMARKS on Form PR. 
4    | Circulating oil/condensate | Original movement off lease
5    | Lost or stolen | Includes loss from fire, leaks, spills, and breaks, as well as theft. File Form H-8 if more than 5 barrels.
6    | Sedimentation | BS&W from commercial tank cleaning. Show net oil /condensate as oil/condensate disposition Code 3. Indicate the name of the tank service and/or R-2 facility in REMARKS on Form PR. 
71   | Other - change of operator |
72   | Other - road oil |
73   | Other - lease use |
74   | Other - production lost to formation |
75   | Other | Provide an explanation in REMARKS on Form PR.
8    | Skim liquid hydrocarbons | Charged back on Form P-18 by a saltwater disposal system.

#### CASINGHEAD/GAS WELL GAS DISPOSITION CODES: Column #12

Code | Definition | Description
-----|------------|------------
1    | Lease or field fuel use | Gas used or given to others for field operations including lease drilling fuel, compressor fuel, etc.
2    | Transmission line | Gas delivered to a transmission line that will not be processed further before ultimate use, including gas used for industrial purposes, irrigation or refinery fuel, etc. 
3    |  Processing plant | Total gas delivered to a gas processing plant (any plant or facility reported on Form R-3). Do not report the “plant breakdown” of the gas on Form PR. 
4    | Vented or flared | Indicate why the gas was vented or flared in REMARKS on Form PR. 
5    |  Gas Lift | Gas you use, sell or give to others directly for gas lift. Do not include gas delivered to pressure maintenance or processing plants even though it is ultimately used for gas lift. 
6    | Repressure or pressure maintenance | Gas delivered to a system or plant that does not extract liquid hydrocarbons. That system or plant will report on Form R-7. (A pressure maintenance plant or system that does extract liquid hydrocarbons must file Form R-3. If gas is delivered to a plant or system that recovers liquid hydrocarbons, use casinghead gas/gas well gas disposition Code 3 even though the gas may ultimately be injected for pressure maintenance.) 
7    | Carbon black | Gas delivered to a carbon black plant. 
8    |  Underground storage | Gas injected directly into a storage reservoir.
