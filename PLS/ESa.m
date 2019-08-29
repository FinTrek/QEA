global p K_max

cvx_solver mosek

IC_needed = 1;
tol = 0.0001;
R = 80;


TradStatFF = wekbindestTradStat;
Index = wekbindestMATdex;
stkcd = wekbindeststkcd;

DaRet  = TradStatFF(:,1);
MKT    = TradStatFF(:,2);
ThrSMB = TradStatFF(:,3);
ThrHML = TradStatFF(:,4);
FivSMB = TradStatFF(:,5);
FivHML = TradStatFF(:,6);
FivRMW = TradStatFF(:,7);
FivCMA = TradStatFF(:,8);

code = Index(:,1);
day  = Index(:,2);

X = [MKT, ThrSMB, ThrHML];
y = DaRet;

%%
p = size(X, 2);
T = 240;
N = length(stkcd);


K_max = 4;
lamb.grid = 10;
lamb.min  = 0.2;
lamb.max  = 2.0;
lamb_const = lamb.min * (lamb.max / lamb.min ).^( ( (1:lamb.grid) - 1) /( lamb.grid -1 ) ); % the constant for lambda. very important!!
numlam = length(lamb_const);


%% shape the data
index = dataset( code, day, y, X );
index.Properties.VarNames = {'N'  'T'  'y'  'X'};

y_raw = y;
X_raw = X;

for i = 1:N
    yi = y(index.N == i);
    mean_yi = mean(yi);
    yi = bsxfun(@minus, yi, mean(yi) );
    y(index.N == i) = yi;
    y_raw(index.N==i) = y(index.N == i) + mean_yi;
    
    Xi = X(index.N == i, : );
    mean_Xi = mean(Xi);
    Xi = bsxfun(@minus, Xi, mean(Xi) );
    X(index.N == i, :) = Xi;
    X_raw(index.N == i, :) = X(index.N == i, :) + repmat( mean(Xi), [T 1]);
end

ds = dataset( code, day, y, X, y_raw, X_raw );
ds.Properties.VarNames = {'N'  'T'  'y'  'X' 'y_raw' 'X_raw'};


%% initial values
beta_hat0 = zeros(N, p);
for i = 1:N
    yi = ds.y(ds.N == i );
    Xi = ds.X(ds.N == i, : );
    beta_hat0(i,:) = regress(yi, Xi);
end

%% estimation
TT = T;
IC_total = ones(K_max, numlam );

if IC_needed == 1
    for ll = 1:numlam
        disp(ll)
        
        a = ds.X \ ds.y; 
        bias = SPJ_PLS(T, ds.y_raw, ds.X_raw);
        a_corr = 2 * a - bias;
        IC_total(1, :) = mean( ( y - X*a_corr ).^2 );
        
        
        for K = 2:K_max
            Q = 999*zeros(K,1);
            
            lam = lamb_const(ll) * var(y) * T^(-1/3); %
            [b_K, hat.a] = PLS_est(N, TT, y, X, beta_hat0, K, lam, R, tol); % estimation
            [~, H.b, ~, group] = report_b( b_K, hat.a, K );
            sum(group)            

            post_b = zeros(N, p);
            post_a = zeros(K, p);
            if K >=2
                for i = 1:K
                    NN = 1:N;
                    H.group = logical(group);
                    this_group = group(:,i);
                    if sum(this_group) > 0
                        g_index = NN(this_group);
                        g_data = ds( ismember(ds.N, g_index), : );

                        post = post_est_PLS_dynamic(T, g_data);
                        
                        e = g_data.y - g_data.X * post.post_a_corr ;
                        Q(i) = sum( e.^2 );
                        post_b(this_group,:) = repmat(post.post_a_corr', [sum(this_group), 1] );
                    end
                end
            end
            
            
            IC_total(K , ll) = sum(Q) / (N*T)
            
        end
    end
    %% calculate the IC
    pen = 2/3 * (N*T)^(-.5) * p .* repmat( (1:K_max)', [1 numlam]);
    IC_final = log(IC_total) + pen;
    disp(IC_final)
end

