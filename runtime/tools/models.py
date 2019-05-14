import numpy as np
import pandas as pd
import subprocess
from collections import OrderedDict
class sp:
    def __init__(self,scenario):
        self.scenario = scenario
        self.options = OrderedDict({
            'isurvival': 'T',
            'iload': 'F',
            'iestimate': 'T',
            'data': 'hrs_final_ref.csv',
            'info': 'info_ref.dat',
            'includeratings': 'F',
            'icomplement': 'T',
            'ihetero': 'T',
            'ijointhetero': 'T',
            'ibargaininghetero': 'F',
            'icorr': 'T',
            'iunitary': 'F',
            'idiscount': 'F',
            'ddiscount': 0.95,
            'idirect': 'F',
            'arf': 0.06,
            'drc': 0.08,
            'reprate': 0.6,
            'ishufheter': 'F',
            'ishufwages': 'F',
            'iblockcomp': 'F',
            'ifixcorr': 'F',
            'dfixrho': 0.5,
            'inoestim': 'F'
        })

        print('Default options: ')
        settings = open('../params/settings_'+self.scenario+'.info','w')
        for key, value in self.options.items():
            print(key,' = ',value)
            settings.write(key+', '+str(value)+' \n')
        settings.close()

    def chgoption(self,item,value):
        if self.options[item]==value:
            print('value already set to ',value)
        else :
            print('changed item ',item,' with value',self.options[item],' to ',value ,' ...')
            self.options[item] = value
            settings = open('../params/settings_'+self.scenario+'.info','w')
            for key, value in self.options.items():
                settings.write(key+', '+str(value)+' \n')
            settings.close()

    def estimate(self):
        self.buff = subprocess.check_output(['./runestimation',self.scenario])
        self.write()
        self.getsim()

    def write(self):
        f = open('../params/parms_'+self.scenario+'.csv')
        buff = f.read()
        f.close()
        buff = buff.split('\n')
        loglike = float(buff[0])
        npar = int(buff[1])
        labels = [None] * npar
        par = [None] * npar
        se = [None] * npar
        for i,line in enumerate(buff[2:-1]):
            labels[i],par[i],se[i] = line.split()
            labels[i],par[i],se[i] = line.split()
        self.params = pd.DataFrame(data={'par': par,'se': se},index=labels)
        self.params['par'].astype('float',inplace=True)
        self.params['se'].astype('float',inplace=True)
        self.loglike = loglike
        table = open('../tex/tables/estimates_'+self.scenario+'.tex','w')
        table.write('\\begin{tabular}{lcc} \n')
        table.write('\\hline\hline \n')
        table.write(' & Males & Females \\\ \n')
        table.write('Own leisure & $\\alpha_{i}^{m}$ & $\\beta_{i}^{f}$ \\\ \n')
        alpha_lm = []
        beta_lf = []
        for i,l in enumerate(labels):
            if 'alpha_lm' in l:
                alpha_lm.append([float(par[i]),float(se[i])])
            if 'beta_lf' in l:
                beta_lf.append([float(par[i]),float(se[i])])
            if 'beta_lm_lf' in l:
                beta_lf.append([float(par[i]),float(se[i])])
        vars = ['constant','age','health limitations','college','spouse responded','satisfied job','leisure spouse']
        for i,v in enumerate(vars):
            table.write(v+' & '+'{:1.3f}'.format(alpha_lm[i][0])+' & '+'{:1.3f}'.format(beta_lf[i][0])+' \\\ \n')
            table.write(' & ('+'{:1.3f}'.format(alpha_lm[i][1])+') & ('+'{:1.3f}'.format(beta_lf[i][1])+') \\\ \n')
        table.write('Consumption & $\\alpha^{c}$ & $\\beta^{c}$ \\\ \n')
        for i,l in enumerate(labels):
            if 'alpha_c' in l:
                alpha_c = [float(par[i]),float(se[i])]
            if 'beta_c' in l:
                beta_c = [float(par[i]),float(se[i])]
        table.write(' & '+'{:1.3f}'.format(alpha_c[0])+' & '+'{:1.3f}'.format(beta_c[0])+' \\\ \n')
        table.write(' & ('+'{:1.3f}'.format(alpha_c[1])+') & ('+'{:1.3f}'.format(beta_c[1])+') \\\ \n')
        table.write('Discount factors & $\\rho^m$ & $\\rho^f$ \\\ \n')
        for i,l in enumerate(labels):
            if 'log_rho_m' in l:
                rho_m = [np.exp(float(par[i])),np.exp(float(par[i]))*float(se[i])]
            if 'log_rho_f' in l:
                rho_f = [np.exp(float(par[i])),np.exp(float(par[i]))*float(se[i])]
        table.write(' & '+'{:1.3f}'.format(rho_m[0])+' & '+'{:1.3f}'.format(rho_f[0])+' \\\ \n')
        table.write(' & ('+'{:1.3f}'.format(rho_m[1])+') & ('+'{:1.3f}'.format(rho_f[1])+') \\\ \n')
        for i,l in enumerate(labels):
            if 'mu' in l:
                mu = [float(par[i]),float(se[i])]
            if 'wageratio' in l:
                wageratio = [float(par[i]),float(se[i])]
        table.write('Weights & $\lambda$ &  \\\ \n')
        table.write('Constant & '+'{:1.3f}'.format(mu[0])+' &  \\\ \n')
        table.write(' & '+'{:1.3f}'.format(mu[1])+' &  \\\ \n')
        table.write('$\log(\\frac{w_m}{w_f})$ & '+'{:1.3f}'.format(wageratio[0])+' &  \\\ \n')
        table.write(' & '+'{:1.3f}'.format(wageratio[1])+' &  \\\ \n')
        table.write('Heterogeneity & $\eta_i^m$ & $\\nu_i^f$ \\\ \n')
        L = np.zeros((2,2))
        for i,l in enumerate(labels):
            if 'L_nm_nm' in l:
                lmm = [float(par[i]),float(se[i])]
            if 'L_nf_nf' in l:
                lff = [float(par[i]),float(se[i])]
            if 'L_nf_nm' in l:
                lmf = [float(par[i]),float(se[i])]
        L[0,0] = lmm[0]
        L[1,0] = lmf[0]
        L[1,1] = lff[0]
        cov = np.matmul(L,np.transpose(L))
        cov[1,0] = cov[1,0]/np.sqrt(cov[0,0]*cov[1,1])
        cov[0,1] = cov[1,0]
        cov[1,1] = np.sqrt(cov[1,1])
        cov[0,0] = np.sqrt(cov[0,0])
        self.cor = cov
        table.write('Std.Dev & '+'{:1.3f}'.format(cov[0,0])+' & '+'{:1.3f}'.format(cov[1,1])+' \\\ \n')
        table.write('Correlation & '+'{:1.3f}'.format(cov[1,0])+' &  \\\ \n')
        table.write('Heterogeneity & $\\zeta_i^m$ & $\\zeta_i^f$ \\\ \n')
        L = np.zeros((2,2))
        for i,l in enumerate(labels):
            if 'LJ_nm_nm' in l:
                lmm = [float(par[i]),float(se[i])]
            if 'LJ_nf_nf' in l:
                lff = [float(par[i]),float(se[i])]
            if 'LJ_nf_nm' in l:
                lmf = [float(par[i]),float(se[i])]
        L[0,0] = lmm[0]
        L[1,0] = lmf[0]
        L[1,1] = lff[0]
        cov = np.matmul(L,np.transpose(L))
        cov[1,0] = cov[1,0]/np.sqrt(cov[0,0]*cov[1,1])
        cov[0,1] = cov[1,0]
        cov[1,1] = np.sqrt(cov[1,1])
        cov[0,0] = np.sqrt(cov[0,0])
        self.corzeta = cov
        table.write('Std.Dev & '+'{:1.3f}'.format(cov[0,0])+' & '+'{:1.3f}'.format(cov[1,1])+' \\\ \n')
        table.write('Correlation & '+'{:1.3f}'.format(cov[1,0])+' &  \\\ \n')
        table.write('\\hline \n')
        table.write('$\\log L$ & '+'{:4.1f}'.format(loglike) +' & \\\ \n')
        table.write('\\hline \hline \n')
        table.write('\\end{tabular} \n')
        table.close()
    def getsim(self):
        vars = ['hhidpn','insim','jprob','rexpret_sim','sexpret_sim','leisure_m', 'leisure_f', 'leisure_joint_m', 'leisure_joint_f', 'uhbargain']
        sim = pd.read_csv('../data/outcomes_'+self.scenario+'.dat',names=vars,sep='\s+')
        self.sim = sim[sim.insim==1]
        hrs = pd.read_stata('../data/hrs_final_ref.dta')
        self.sim = self.sim.merge(hrs,on='hhidpn')
