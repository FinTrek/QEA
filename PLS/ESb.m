%% PLS estimation
K = 2;
lam = 0.3336 *var(y) * T^(-1/3);

[b_K, a] = PLS_est(N, T, y, X, beta_hat0, K, lam, R, tol);
[~, b, ~ , group] = report_b( b_K, a, K );

%% post estimation
num = K*3
est_lasso = zeros(p, num);
est_post_lasso = zeros(p, num);

for i = 1:K
    NN = 1:N;
    group = logical(group);
    this_group = group(:,i);
    g_index = NN(this_group);
    g_data = ds( ismember(ds.N, g_index), : ); % group-specific data
    post = post_est_PLS_dynamic(T, g_data);
    est_post_lasso(:,(3*i-2):(3*i)) =  [post.post_a_corr, post.se, post.test_b];
end

%% display the estimates
est_post_lasso = mat2dataset( est_post_lasso, 'VarNames', ...
    {'g1_coef', 'g1_sd', 'g1_t', 'g2_coef', 'g2_sd', 'g2_t'});
disp(est_post_lasso)

stkcd(group(:,1))
stkcd(group(:,2))
% stkcd(group(:,3))

g_PLS = zeros(length(stkcd),1);
g_PLS( group(:,1) == 1 ) = 1;
g_PLS( group(:,2) == 1 ) = 2;
% g_PLS( group(:,3) == 1 ) = 3;

%% common FE

g_index = NN;
first_none_zero = min( NN );
g_data = ds( ismember(ds.N, g_index), : ); % group-specific data
post = post_est_PLS_dynamic(T, g_data);

[post.post_a_corr, post.se, post.test_b]

%% save the result

save group.mat 'g_PLS'
est_post_lasso=double(est_post_lasso)
save est_PLS.mat 'est_post_lasso'