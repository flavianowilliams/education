set table "conservacao_da_energia-ES.tkzfct.table"; set format "%.5f"
set samples 200.0; plot [x=0:12.000000000000000000] (2*exp(-(x-2.5)**2)-2*exp(-(x-5)**2)-3*exp(-(x-8.5)**2)+3)/1
