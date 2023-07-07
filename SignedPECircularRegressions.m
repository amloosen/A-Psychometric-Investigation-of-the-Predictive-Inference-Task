% Implement the Circular Regression by Nassar et al.
% AL; May 2022
clear all; close all

%% Load data
rep = ('./R/Data/');
Game_pt1 = readtable([rep 'finalGameQuestData_pt1Mod.csv'],'Delimiter',{','});
Game_pt1.Var1 = [];
Game_pt1_odd = readtable([rep 'finalGameQuest_pt1_oddMod.csv'],'Delimiter',{','});
Game_pt1_odd.Var1 = [];
Game_pt1_even = readtable([rep 'finalGameQuest_pt1_evenMod.csv'],'Delimiter',{','});
Game_pt1_even.Var1 = [];

%t2
Game_t2 = readtable([rep 'finalGameQuestData_t2Mod.csv'],'Delimiter',{','});
Game_t2.Var1 = [];
Game_t2_odd = readtable([rep 'finalGameQuest_t2_oddMod.csv'],'Delimiter',{','});
Game_t2_odd.Var1 = [];
Game_t2_even = readtable([rep 'finalGameQuest_t2_evenMod.csv'],'Delimiter',{','});
Game_t2_even.Var1 = [];

%% Compute circular regressions
%Action-Bayesian Learner Coupling
addpath './CircStat2012a'

unifMix = 0;
unique_ids = unique(Game_pt1.id);
data_sets = {Game_pt1 Game_pt1_odd Game_pt1_even ...
    Game_t2 Game_t2_odd Game_t2_even};

for t=1:2 %two time points

    idx = t+2*(t-1);
    unique_ids = unique(data_sets{idx}.id);

    %initialize data frames
    params = nan(1,7);
    paramsCPP  = nan(1,6);
    paramsPE  = nan(1,6);
    paramsCPPodd  = nan(1,6);
    paramsPEodd  = nan(1,6);
    paramsCPPeven  = nan(1,6);
    paramsPEeven  = nan(1,6);
    paramsodd = nan(1,7);
    paramseven = nan(1,7);

    corr_vecs = [];
    for i=1:length(unique_ids)
        data_tmp = data_sets{idx}(data_sets{idx}.id==unique_ids(i),:);
        data_tmpodd = data_sets{idx+1}(data_sets{idx+1}.id==unique_ids(i),:);
        data_tmpeven = data_sets{idx+2}(data_sets{idx+2}.id==unique_ids(i),:);

        %% prepare data
        data_tmp.behav_d(isnan(data_tmp.behav_d)) = 0;
        %odd
        data_tmpodd.behav_d(isnan(data_tmpodd.behav_d)) = 0;
        %even
        data_tmpeven.behav_d(isnan(data_tmpeven.behav_d)) = 0;
       
        %% 3.Action-Bayesian Learner Coupling
        rawX = [ones(size(data_tmp.behav_d)),data_tmp.behav_d, data_tmp.behav_d.*meanCentX(data_tmp.bayes_RU),...
            data_tmp.behav_d.*meanCentX(data_tmp.bayes_CPP),data_tmp.behav_d.*meanCentX(data_tmp.hitMiss)];
        sel3=isfinite(data_tmp.actionUpdate) & all(isfinite(rawX), 2);

        data.Y =  data_tmp.actionUpdate(sel3);
        data.X = (rawX(sel3,:));
        data.priorWidth = ones(1, size((rawX(sel3,:)), 2)).*5;
        data.priorMean = [0, 0.5 , zeros(1, 3)];
        data.startPoint = [5, 0, 0, 0, 0,0, unifMix];
        data.includeUniform = true;
        data.whichParams = logical([1, 1, 1, 1, 1,1, 0]);
        params(i,1) = unique_ids(i);
        [params(i,2:end), ~] = fitLinearModWCircErrs(data);
        
        %odd
        rawXodd = [ones(size(data_tmpodd.behav_d)), data_tmpodd.behav_d, data_tmpodd.behav_d.*meanCentX(data_tmpodd.bayes_CPP), ...
            data_tmpodd.behav_d.*meanCentX(data_tmpodd.bayes_RU), data_tmpodd.behav_d.*meanCentX(data_tmpodd.hitMiss)];
        sel3odd=isfinite(data_tmpodd.actionUpdate) & all(isfinite(rawXodd), 2);

        dataodd.Y =  data_tmpodd.actionUpdate(sel3odd);
        dataodd.X = (rawXodd(sel3odd,:));
        dataodd.priorWidth = ones(1, size((rawXodd(sel3odd,:)), 2)).*5;
        dataodd.priorMean = [0, .5, zeros(1, 3)];
        dataodd.startPoint = [5, 0, 0, 0, 0, 0, unifMix];
        dataodd.includeUniform = true;
        dataodd.whichParams = logical([1, 1, 1, 1, 1, 1, 0]);
        paramsodd(i,1) = unique_ids(i);
        [paramsodd(i,2:end), ~] = fitLinearModWCircErrs(dataodd);

        %even
        rawXeven = [ones(size(data_tmpeven.behav_d)), data_tmpeven.behav_d, data_tmpeven.behav_d.*meanCentX(data_tmpeven.bayes_CPP), ...
            data_tmpeven.behav_d.*meanCentX(data_tmpeven.bayes_RU), data_tmpeven.behav_d.*meanCentX(data_tmpeven.hitMiss)];
        sel3even=isfinite(data_tmpeven.actionUpdate) & all(isfinite(rawXeven), 2);

        dataeven.Y =  data_tmpeven.actionUpdate(sel3even);
        dataeven.X = (rawXeven(sel3even,:));
        dataeven.priorWidth = ones(1, size((rawXeven(sel3even,:)), 2)).*5;
        dataeven.priorMean = [0, .5, zeros(1, 3)];
        dataeven.startPoint = [5, 0, 0, 0, 0, 0, unifMix];
        dataeven.includeUniform = true;
        dataeven.whichParams = logical([1, 1, 1, 1, 1, 1, 0]);
        paramseven(i,1) = unique_ids(i);
        [paramseven(i,2:end), ~] = fitLinearModWCircErrs(dataeven);

        %PE
        rawXPE = [ones(size(data_tmp.behav_d)), data_tmp.behav_d, ...
            data_tmp.behav_d.*meanCentX(data_tmp.bayes_RU), data_tmp.behav_d.*meanCentX(data_tmp.hitMiss)];
        sel3PE=isfinite(data_tmp.actionUpdate) & all(isfinite(rawXPE), 2);

        dataPE.Y =  data_tmp.actionUpdate(sel3PE);
        dataPE.X = (rawXPE(sel3,:));
        dataPE.priorWidth = ones(1, size((rawXPE(sel3PE,:)), 2)).*5;
        dataPE.priorMean = [0, 0.5, zeros(1, 2)];
        dataPE.startPoint = [5,  0, 0, 0, 0, unifMix];%not sure
        dataPE.includeUniform = true;
        dataPE.whichParams = logical([1, 1, 1,  1, 1, 0]);
        paramsPE(i,1) = unique_ids(i);
        [paramsPE(i,2:end), ~] = fitLinearModWCircErrs(dataPE);
       %odd
        rawXPEodd = [ones(size(data_tmpodd.behav_d)), meanCentX(data_tmpodd.behav_d), ...
            meanCentX(data_tmpodd.behav_d).*meanCentX(data_tmpodd.bayes_RU), meanCentX(data_tmpodd.behav_d).*meanCentX(data_tmpodd.hitMiss)];
        sel3PEodd=isfinite(data_tmpodd.actionUpdate) & all(isfinite(rawXPEodd), 2);

        dataPEodd.Y =  data_tmpodd.actionUpdate(sel3PEodd);
        dataPEodd.X = (rawXPEodd(sel3PEodd,:));
        dataPEodd.priorWidth = ones(1, size((rawXPEodd(sel3PEodd,:)), 2)).*5;
        dataPEodd.priorMean = [0, 0.5, zeros(1, 2)];
        dataPEodd.startPoint = [5,  0, 0, 0, 0, unifMix];
        dataPEodd.includeUniform = true;
        dataPEodd.whichParams = logical([1, 1, 1,  1, 1, 0]);
        paramsPEodd(i,1) = unique_ids(i);
        [paramsPEodd(i,2:end), ~] = fitLinearModWCircErrs(dataPEodd);
      
         %even
        rawXPEeven = [ones(size(data_tmpeven.behav_d)), data_tmpeven.behav_d, ...
            data_tmpeven.behav_d.*meanCentX(data_tmpeven.bayes_RU), data_tmpeven.behav_d.*meanCentX(data_tmpeven.hitMiss)];
        sel3PEeven=isfinite(data_tmpeven.actionUpdate) & all(isfinite(rawXPEeven), 2);

        dataPEeven.Y =  data_tmpeven.actionUpdate(sel3PEeven);
        dataPEeven.X = (rawXPEeven(sel3PEeven,:));
        dataPEeven.priorWidth = ones(1, size((rawXPEeven(sel3PEeven,:)), 2)).*5;
        dataPEeven.priorMean = [0, 0.5, zeros(1, 2)];
        dataPEeven.startPoint = [5,  0, 0, 0, 0, unifMix];
        dataPEeven.includeUniform = true;
        dataPEeven.whichParams = logical([1, 1, 1,  1, 1, 0]);
        paramsPEeven(i,1) = unique_ids(i);
        [paramsPEeven(i,2:end), ~] = fitLinearModWCircErrs(dataPEeven);
      
        %CPP
        rawXCPP = [ones(size(data_tmp.behav_d)), data_tmp.behav_d.*meanCentX(data_tmp.bayes_CPP), ...
            data_tmp.behav_d.*meanCentX(data_tmp.bayes_RU), data_tmp.behav_d.*meanCentX(data_tmp.hitMiss)];
        sel3CPP=isfinite(data_tmp.actionUpdate) & all(isfinite(rawXCPP), 2);

        dataCPP.Y =  data_tmp.actionUpdate(sel3CPP);
        dataCPP.X = (rawXCPP(sel3CPP,:));
        dataCPP.priorWidth = ones(1, size((rawXCPP(sel3CPP,:)), 2)).*5;
        dataCPP.priorMean = [0, zeros(1, 3)];
        dataCPP.startPoint = [5,  0, 0, 0, 0, unifMix];%not sure
        dataCPP.includeUniform = true;
        dataCPP.whichParams = logical([1,  1, 1, 1, 1, 0]);
        paramsCPP(i,1) = unique_ids(i);
        [paramsCPP(i,2:end), ~] = fitLinearModWCircErrs(dataCPP);
       
        %odd
        rawXCPPodd = [ones(size(data_tmpodd.behav_d)), data_tmpodd.behav_d.*meanCentX(data_tmpodd.bayes_CPP), ...
            data_tmpodd.behav_d.*meanCentX(data_tmpodd.bayes_RU), data_tmpodd.behav_d.*meanCentX(data_tmpodd.hitMiss)];
        sel3CPPodd=isfinite(data_tmpodd.actionUpdate) & all(isfinite(rawXCPPodd), 2);

        dataCPPodd.Y =  data_tmpodd.actionUpdate(sel3CPPodd);
        dataCPPodd.X = (rawXCPPodd(sel3CPPodd,:));
        dataCPPodd.priorWidth = ones(1, size((rawXCPPodd(sel3CPPodd,:)), 2)).*5;
        dataCPPodd.priorMean = [0, 0.5, zeros(1, 2)];
        dataCPPodd.startPoint = [5,  0, 0, 0, 0, unifMix];
        dataCPPodd.includeUniform = true;
        dataCPPodd.whichParams = logical([1, 1, 1,  1, 1, 0]);
        paramsCPPodd(i,1) = unique_ids(i);
        [paramsCPPodd(i,2:end), ~] = fitLinearModWCircErrs(dataCPPodd);
      
         %even
        rawXCPPeven = [ones(size(data_tmpeven.behav_d)), data_tmpeven.behav_d.*meanCentX(data_tmpeven.bayes_CPP), ...
            data_tmpeven.behav_d.*meanCentX(data_tmpeven.bayes_RU), data_tmpeven.behav_d.*meanCentX(data_tmpeven.hitMiss)];
        sel3CPPeven=isfinite(data_tmpeven.actionUpdate) & all(isfinite(rawXCPPeven), 2);

        dataCPPeven.Y =  data_tmpeven.actionUpdate(sel3CPPeven);
        dataCPPeven.X = (rawXCPPeven(sel3CPPeven,:));
        dataCPPeven.priorWidth = ones(1, size((rawXCPPeven(sel3CPPeven,:)), 2)).*5;
        dataCPPeven.priorMean = [0, 0.5, zeros(1, 2)];
        dataCPPeven.startPoint = [5,  0, 0, 0, 0, unifMix];
        dataCPPeven.includeUniform = true;
        dataCPPeven.whichParams = logical([1, 1, 1,  1, 1, 0]);
        paramsCPPeven(i,1) = unique_ids(i);
        [paramsCPPeven(i,2:end), ~] = fitLinearModWCircErrs(dataCPPeven);
      
    end
    params = array2table(params);
    params.Properties.VariableNames = {'id','Conc','Intercept','PE','RU','CPP', 'Hit'};

    paramseven = array2table(paramseven);
    paramseven.Properties.VariableNames = {'id','Conc','Intercept','PE','CPP','RU', 'Hit'};

    paramsodd = array2table(paramsodd);
    paramsodd.Properties.VariableNames = {'id','Conc','Intercept','PE','CPP','RU', 'Hit'};
    
    paramsPE = array2table(paramsPE);
    paramsPE.Properties.VariableNames = {'id','Conc','Intercept','PE','RU', 'Hit'};

    paramsCPP = array2table(paramsCPP);
    paramsCPP.Properties.VariableNames = {'id','Conc','Intercept','CPP','RU', 'Hit'};

    paramsPEodd = array2table(paramsPEodd);
    paramsPEodd.Properties.VariableNames = {'id','Conc','Intercept','PE','RU', 'Hit'};

    paramsCPPodd = array2table(paramsCPPodd);
    paramsCPPodd.Properties.VariableNames = {'id','Conc','Intercept','CPP','RU', 'Hit'};

    paramsPEeven = array2table(paramsPEeven);
    paramsPEeven.Properties.VariableNames = {'id','Conc','Intercept','PE','RU', 'Hit'};

    paramsCPPeven = array2table(paramsCPPeven);
    paramsCPPeven.Properties.VariableNames = {'id','Conc','Intercept','CPP','RU', 'Hit'};

    params_alltimepoints{t} = params;
    paramsodd_alltimepoints{t} = paramsodd;
    paramseven_alltimepoints{t} = paramseven;
    paramsPE_alltimepoints{t} = paramsPE;
    paramsCPP_alltimepoints{t} = paramsCPP;
    paramsPEodd_alltimepoints{t} = paramsPEodd;
    paramsCPPodd_alltimepoints{t} = paramsCPPodd;
    paramsPEeven_alltimepoints{t} = paramsPEeven;
    paramsCPPeven_alltimepoints{t} = paramsCPPeven;
end

%% save data
writetable(params_alltimepoints{1}, 'R/Data/CircularBetas_t1.csv')
writetable(params_alltimepoints{2}, 'R/Data/CircularBetas_t2.csv')

writetable(paramsodd_alltimepoints{1}, 'R/Data/CircularBetasOdd_t1.csv')
writetable(paramsodd_alltimepoints{2}, 'R/Data/CircularBetasOdd_t2.csv')

writetable(paramseven_alltimepoints{1}, 'R/Data/CircularBetasEven_t1.csv')
writetable(paramseven_alltimepoints{2}, 'R/Data/CircularBetasEven_t2.csv')

writetable(paramsPE_alltimepoints{1}, 'R/Data/CircularBetasPE_t1.csv')
writetable(paramsPE_alltimepoints{2}, 'R/Data/CircularBetasPE_t2.csv')

writetable(paramsPEodd_alltimepoints{1}, 'R/Data/CircularBetasOddPE_t1.csv')
writetable(paramsPEodd_alltimepoints{2}, 'R/Data/CircularBetasOddPE_t2.csv')

writetable(paramsPEeven_alltimepoints{1}, 'R/Data/CircularBetasEvenPE_t1.csv')
writetable(paramsPEeven_alltimepoints{2}, 'R/Data/CircularBetasEvenPE_t2.csv')

writetable(paramsCPP_alltimepoints{1}, 'R/Data/CircularBetasCPP_t1.csv')
writetable(paramsCPP_alltimepoints{2}, 'R/Data/CircularBetasCPP_t2.csv')

writetable(paramsCPPodd_alltimepoints{1}, 'R/Data/CircularBetasOddCPP_t1.csv')
writetable(paramsCPPodd_alltimepoints{2}, 'R/Data/CircularBetasOddCPP_t2.csv')

writetable(paramsCPPeven_alltimepoints{1}, 'R/Data/CircularBetasEvenCPP_t1.csv')
writetable(paramsCPPeven_alltimepoints{2}, 'R/Data/CircularBetasEvenCPP_t2.csv')
