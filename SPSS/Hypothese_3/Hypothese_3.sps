* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
WEIGHT BY weight.

NPAR TESTS
  /M-W= Q11.2_numeric BY IstZGoderSH_numeric(1 2)
  /MISSING ANALYSIS.

T-TEST GROUPS=IstZGoderSH_numeric(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=Q11.2_numeric
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).
