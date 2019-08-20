% reshape data

TradStatFF = FF5wkdTradStat;
DaRet  = TradStatFF(:,1);
MKT    = TradStatFF(:,2);
ThrSMB = TradStatFF(:,3);
ThrHML = TradStatFF(:,4);
FivSMB = TradStatFF(:,5);
FivHML = TradStatFF(:,6);
FivRMW = TradStatFF(:,7);
FivCMA = TradStatFF(:,8);

Index = FF5wkdMATdex;
code = Index(:,1);
day  = Index(:,2);

stkcd = FF5wkdstkcd;

%%
clear('FF5wkdTradStat', 'FF5wkdMATdex', 'FF5wkdstkcd', 'TradStatFF', 'Index')

%%

save QEAFF.mat
