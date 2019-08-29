% reshape data

TradStatFF = wekbindTradStat;
DaRet  = TradStatFF(:,1);
MKT    = TradStatFF(:,2);
ThrSMB = TradStatFF(:,3);
ThrHML = TradStatFF(:,4);
FivSMB = TradStatFF(:,5);
FivHML = TradStatFF(:,6);
FivRMW = TradStatFF(:,7);
FivCMA = TradStatFF(:,8);

Index = wekbindMATdex;
code = Index(:,1);
day  = Index(:,2);

stkcd = wekbindstkcd;

%%
clear('wekdTradStat', 'wkdMATdex', 'wkdstkcd', 'TradStatFF', 'Index')

%%

save QEAFF.mat
