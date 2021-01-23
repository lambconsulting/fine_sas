
citation("survival")


library(survival)
#sas <- read.csv("E:/Lamb_KL_PERS/Consulting/Fine/Fine_SAS_071313/Fine_SAS_Multi_071313/SAS_Condition_072013.csv")
sas <- read.csv("U:/Consulting/KEL/Fine/Fine_SAS_071313/Fine_SAS_Multi_071313/SAS_Pre080513/SAS_Condition_072013.csv")

names(sas) <- tolower(names(sas))
ls(sas)
sas_severe = sas(cond="severe")

hist(sas$age)
hist(sas$pg)

#AGE ATENOLOL
#atplot <- coxph(Surv(t,e==1)~  pspline(age, df = 4), data=aten)
#termplot(atplot, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
#         xlab = "Age", main = "Age Post-Tx Survial")

acplot <- function(v,ds,dst) {
  v.form <- as.formula(paste("Surv(t,e==1) ~ pspline(",v,",df=4)",sep=""))
  ph.fit <- coxph(v.form,data=ds)
  termplot(ph.fit,term=1,se=TRUE,ylab='Log Hazard',rug=TRUE,xlab=v,main=dst)
}

acplot("age",sas,"age")
acplot("pg",sas,"pg")

pdf("plots.pdf")
termplot(.)
termplot(.)
termplot(.)
dev.off()



cardplot <- function(v,ds,dst) {
  v.form <- as.formula(paste("Surv(t,card_event==1) ~ pspline(",v,",df=4)",sep=""))
  ph.fit <- coxph(v.form,data=ds)
  termplot(ph.fit,term=1,se=TRUE,ylab='Log Hazard',rug=TRUE,xlab=v,main=dst)
}

cardplot("wt_kg",cat,"cat")
cardplot("age_y",cat,"cat")
cardplot("troponin_i",cat,"cat")
cardplot("t4",cat,"cat")
cardplot("crt",cat,"cat")
cardplot("bun",cat,"cat")
cardplot("lvc_echo",cat,"cat")
cardplot("lvs_echo",cat,"cat")
cardplot("fs_echo",cat,"cat")
cardplot("lvwd_echo",cat,"cat")
cardplot("ivsd_echo",cat,"cat")
cardplot("lamm_echo",cat,"cat")
cardplot("aomm_echo",cat,"cat")
cardplot("laao_echo",cat,"cat")
cardplot("bnp",cat,"cat")
cardplot("bnp_l",cat,"cat")
cardplot("wt_kg",chf,"chf")
cardplot("age_y",chf,"chf")
cardplot("troponin_i",chf,"chf")
cardplot("t4",chf,"chf")
cardplot("crt",chf,"chf")
cardplot("bun",chf,"chf")
cardplot("lvc_echo",chf,"chf")
cardplot("lvs_echo",chf,"chf")
cardplot("fs_echo",chf,"chf")
cardplot("lvwd_echo",chf,"chf")
cardplot("ivsd_echo",chf,"chf")
cardplot("lamm_echo",chf,"chf")
cardplot("aomm_echo",chf,"chf")
cardplot("laao_echo",chf,"chf")
cardplot("bnp",chf,"chf")
cardplot("bnp_l",chf,"chf")
cardplot("wt_kg",normal,"normal")
cardplot("age_y",normal,"normal")
cardplot("troponin_i",normal,"normal")
cardplot("t4",normal,"normal")
cardplot("crt",normal,"normal")
cardplot("bun",normal,"normal")
cardplot("lvc_echo",normal,"normal")
cardplot("lvs_echo",normal,"normal")
cardplot("fs_echo",normal,"normal")
cardplot("lvwd_echo",normal,"normal")
cardplot("ivsd_echo",normal,"normal")
cardplot("lamm_echo",normal,"normal")
cardplot("aomm_echo",normal,"normal")
cardplot("laao_echo",normal,"normal")
cardplot("bnp",normal,"normal")
cardplot("bnp_l",normal,"normal")
cardplot("wt_kg",ocm,"ocm")
cardplot("age_y",ocm,"ocm")
cardplot("troponin_i",ocm,"ocm")
cardplot("t4",ocm,"ocm")
cardplot("crt",ocm,"ocm")
cardplot("bun",ocm,"ocm")
cardplot("lvc_echo",ocm,"ocm")
cardplot("lvs_echo",ocm,"ocm")
cardplot("fs_echo",ocm,"ocm")
cardplot("lvwd_echo",ocm,"ocm")
cardplot("ivsd_echo",ocm,"ocm")
cardplot("lamm_echo",ocm,"ocm")
cardplot("aomm_echo",ocm,"ocm")
cardplot("laao_echo",ocm,"ocm")
cardplot("bnp",ocm,"ocm")
cardplot("bnp_l",ocm,"ocm")
cardplot("age_y",resp,"resp")
cardplot("troponin_i",resp,"resp")
cardplot("t4",resp,"resp")
cardplot("crt",resp,"resp")
cardplot("bun",resp,"resp")
cardplot("lvc_echo",resp,"resp")
cardplot("lvs_echo",resp,"resp")
cardplot("fs_echo",resp,"resp")
cardplot("lvwd_echo",resp,"resp")
cardplot("ivsd_echo",resp,"resp")
cardplot("lamm_echo",resp,"resp")
cardplot("aomm_echo",resp,"resp")
cardplot("laao_echo",resp,"resp")
cardplot("bnp",resp,"resp")
cardplot("bnp_l",resp,"resp")



# All Patients #
bnp_ln <- coxph(Surv(t,ac_event==1)~  pspline(bnp_l, df = 4), data=cat)
termplot(bnp_ln, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Normalized", main = "ln Post-Tx AC Survial All Patients")

bnp_raw <- coxph(Surv(t,ac_event==1)~  pspline(bnp, df = 4), data=cat)
termplot(bnp_raw, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Raw", main = "Raw Post-Tx AC Survial All Patients")

# CHF Patients #
chf_ln <- coxph(Surv(t,ac_event==1)~  pspline(bnp_l, df = 4), data=chf)
termplot(chf_ln, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Normalized", main = "ln Post-Tx AC Survial chf Patients")

chf_raw <- coxph(Surv(t,ac_event==1)~  pspline(bnp, df = 4), data=chf)
termplot(chf_raw, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Raw", main = "Raw Post-Tx AC Survial chf Patients")

# Normal Patients #
normal_ln <- coxph(Surv(t,ac_event==1)~  pspline(bnp_l, df = 3), data=normal)
termplot(normal_ln, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Normalized", main = "ln Post-Tx AC Survial normal Patients")

normal_raw <- coxph(Surv(t,ac_event==1)~  pspline(bnp, df = 3), data=normal)
termplot(normal_raw, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Raw", main = "Raw Post-Tx AC Survial normal Patients")

# Ocm Patients #
ocm_ln <- coxph(Surv(t,ac_event==1)~  pspline(bnp_l, df = 4), data=ocm)
termplot(ocm_ln, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Normalized", main = "ln Post-Tx AC Survial ocm Patients")

ocm_raw <- coxph(Surv(t,ac_event==1)~  pspline(bnp, df = 4), data=ocm)
termplot(ocm_raw, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Raw", main = "Raw Post-Tx AC Survial ocm Patients")

# Resp Patients #
resp_ln <- coxph(Surv(t,ac_event==1)~  pspline(bnp_l, df = 4), data=resp)
termplot(resp_ln, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Normalized", main = "ln Post-Tx AC Survial resp Patients")

resp_raw <- coxph(Surv(t,ac_event==1)~  pspline(bnp, df = 4), data=resp)
termplot(resp_raw, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Raw", main = "Raw Post-Tx AC Survial resp Patients")


# Weight in kg#
all_wt <- coxph(Surv(t,ac_event==1)~  pspline(wt_kg, df = 4), data=cat)
termplot(all_wt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Wt", main = "Weight kg Post-Tx AC Survial All Patients")

chf_wt <- coxph(Surv(t,ac_event==1)~  pspline(wt_kg, df = 4), data=chf)
termplot(chf_wt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Wt", main = "Weight kg Post-Tx AC Survial chf Patients")

normal_wt <- coxph(Surv(t,ac_event==1)~  pspline(wt_kg, df = 4), data=normal)
termplot(normal_wt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Wt", main = "Weight kg Post-Tx AC Survial normal Patients")

ocm_wt <- coxph(Surv(t,ac_event==1)~  pspline(wt_kg, df = 4), data=ocm)
termplot(ocm_wt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Wt", main = "Weight kg Post-Tx AC Survial ocm Patients")

resp_wt <- coxph(Surv(t,ac_event==1)~  pspline(wt_kg, df = 4), data=resp)
termplot(resp_wt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Wt", main = "Weight kg Post-Tx AC Survial resp Patients")



# Age in Years #
all_age <- coxph(Surv(t,ac_event==1)~  pspline(age_y, df = 4), data=cat)
termplot(all_age, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Age", main = "Age Years Post-Tx AC Survial All Patients")

chf_age <- coxph(Surv(t,ac_event==1)~  pspline(age_y, df = 4), data=chf)
termplot(chf_age, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Age", main = "Age Years Post-Tx AC Survial chf Patients")

normal_age <- coxph(Surv(t,ac_event==1)~  pspline(age_y, df = 4), data=normal)
termplot(normal_age, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Age", main = "Age Years Post-Tx AC Survial normal Patients")

ocm_age <- coxph(Surv(t,ac_event==1)~  pspline(age_y, df = 4), data=ocm)
termplot(ocm_age, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Age", main = "Age Years Post-Tx AC Survial ocm Patients")

resp_age <- coxph(Surv(t,ac_event==1)~  pspline(age_y, df = 4), data=resp)
termplot(resp_age, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Age", main = "Age Years Post-Tx AC Survial resp Patients")

# Troponin I #
all_troponin <- coxph(Surv(t,ac_event==1)~  pspline(troponin_i, df = 4), data=cat)
termplot(all_troponin, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Tropinin", main = "Tropinin Post-Tx AC Survial All Patients")

chf_troponin <- coxph(Surv(t,ac_event==1)~  pspline(troponin_i, df = 4), data=chf)
termplot(chf_troponin, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Tropinin", main = "Tropinin Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_troponin <- coxph(Surv(t,ac_event==1)~  pspline(troponin_i, df = 5), data=normal)
termplot(normal_troponin, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Tropinin", main = "Tropinin Post-Tx AC Survial normal Patients")

ocm_troponin <- coxph(Surv(t,ac_event==1)~  pspline(troponin_i, df = 4), data=ocm)
termplot(ocm_troponin, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Tropinin", main = "Tropinin Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_troponin <- coxph(Surv(t,ac_event==1)~  pspline(troponin_i, df = 4), data=resp)
termplot(resp_troponin, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "Tropinin", main = "Tropinin Post-Tx AC Survial resp Patients")




# T4 #
all_t4 <- coxph(Surv(t,ac_event==1)~  pspline(t4, df = 4), data=cat)
termplot(all_t4, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "T4", main = "T4 Post-Tx AC Survial All Patients")

chf_t4 <- coxph(Surv(t,ac_event==1)~  pspline(t4, df = 4), data=chf)
termplot(chf_t4, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "T4", main = "T4 Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_t4 <- coxph(Surv(t,ac_event==1)~  pspline(t4, df = 5), data=normal)
termplot(normal_t4, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "T4", main = "T4 Post-Tx AC Survial normal Patients")

ocm_t4 <- coxph(Surv(t,ac_event==1)~  pspline(t4, df = 4), data=ocm)
termplot(ocm_t4, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "T4", main = "T4 Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_t4 <- coxph(Surv(t,ac_event==1)~  pspline(t4, df = 4), data=resp)
termplot(resp_t4, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "T4", main = "T4 Post-Tx AC Survial resp Patients")



# CRT #
all_crt <- coxph(Surv(t,ac_event==1)~  pspline(crt, df = 4), data=cat)
termplot(all_crt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "crt", main = "crt Post-Tx AC Survial All Patients")

chf_crt <- coxph(Surv(t,ac_event==1)~  pspline(crt, df = 4), data=chf)
termplot(chf_crt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "crt", main = "crt Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_crt <- coxph(Surv(t,ac_event==1)~  pspline(crt, df = 5), data=normal)
termplot(normal_crt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "crt", main = "crt Post-Tx AC Survial normal Patients")

ocm_crt <- coxph(Surv(t,ac_event==1)~  pspline(crt, df = 4), data=ocm)
termplot(ocm_crt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "crt", main = "crt Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_crt <- coxph(Surv(t,ac_event==1)~  pspline(crt, df = 4), data=resp)
termplot(resp_crt, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "crt", main = "crt Post-Tx AC Survial resp Patients")


# BUN #
all_bun <- coxph(Surv(t,ac_event==1)~  pspline(bun, df = 4), data=cat)
termplot(all_bun, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "bun", main = "bun Post-Tx AC Survial All Patients")

chf_bun <- coxph(Surv(t,ac_event==1)~  pspline(bun, df = 4), data=chf)
termplot(chf_bun, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "bun", main = "bun Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_bun <- coxph(Surv(t,ac_event==1)~  pspline(bun, df = 5), data=normal)
termplot(normal_bun, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "bun", main = "bun Post-Tx AC Survial normal Patients")

ocm_bun <- coxph(Surv(t,ac_event==1)~  pspline(bun, df = 4), data=ocm)
termplot(ocm_bun, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "bun", main = "bun Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_bun <- coxph(Surv(t,ac_event==1)~  pspline(bun, df = 4), data=resp)
termplot(resp_bun, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "bun", main = "BUN Post-Tx AC Survial resp Patients")

 


# LVC_ECHO #
all_lvc_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvc_echo, df = 4), data=cat)
termplot(all_lvc_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvc_echo", main = "lvc_echo Post-Tx AC Survial All Patients")

chf_lvc_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvc_echo, df = 4), data=chf)
termplot(chf_lvc_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvc_echo", main = "lvc_echo Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_lvc_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvc_echo, df = 5), data=normal)
termplot(normal_lvc_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvc_echo", main = "lvc_echo Post-Tx AC Survial normal Patients")

ocm_lvc_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvc_echo, df = 4), data=ocm)
termplot(ocm_lvc_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvc_echo", main = "lvc_echo Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_lvc_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvc_echo, df = 4), data=resp)
termplot(resp_lvc_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvc_echo", main = "lvc_echo Post-Tx AC Survial resp Patients")


# LVS_ECHO #
all_lvs_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvs_echo, df = 4), data=cat)
termplot(all_lvs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvs_echo", main = "lvs_echo Post-Tx AC Survial All Patients")

chf_lvs_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvs_echo, df = 4), data=chf)
termplot(chf_lvs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvs_echo", main = "lvs_echo Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_lvs_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvs_echo, df = 5), data=normal)
termplot(normal_lvs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvs_echo", main = "lvs_echo Post-Tx AC Survial normal Patients")

ocm_lvs_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvs_echo, df = 4), data=ocm)
termplot(ocm_lvs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvs_echo", main = "lvs_echo Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_lvs_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvs_echo, df = 4), data=resp)
termplot(resp_lvs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvs_echo", main = "lvs_echo Post-Tx AC Survial resp Patients")

# FS_ECHO #
all_fs_echo <- coxph(Surv(t,ac_event==1)~  pspline(fs_echo, df = 4), data=cat)
termplot(all_fs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "fs_echo", main = "fs_echo Post-Tx AC Survial All Patients")

chf_fs_echo <- coxph(Surv(t,ac_event==1)~  pspline(fs_echo, df = 4), data=chf)
termplot(chf_fs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "fs_echo", main = "fs_echo Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_fs_echo <- coxph(Surv(t,ac_event==1)~  pspline(fs_echo, df = 5), data=normal)
termplot(normal_fs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "fs_echo", main = "fs_echo Post-Tx AC Survial normal Patients")

ocm_fs_echo <- coxph(Surv(t,ac_event==1)~  pspline(fs_echo, df = 4), data=ocm)
termplot(ocm_fs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "fs_echo", main = "fs_echo Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_fs_echo <- coxph(Surv(t,ac_event==1)~  pspline(fs_echo, df = 4), data=resp)
termplot(resp_fs_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "fs_echo", main = "fs_echo Post-Tx AC Survial resp Patients")

# LVWD_ECHO #
all_lvwd_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvwd_echo, df = 4), data=cat)
termplot(all_lvwd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvwd_echo", main = "lvwd_echo Post-Tx AC Survial All Patients")

chf_lvwd_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvwd_echo, df = 4), data=chf)
termplot(chf_lvwd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvwd_echo", main = "lvwd_echo Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_lvwd_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvwd_echo, df = 5), data=normal)
termplot(normal_lvwd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvwd_echo", main = "lvwd_echo Post-Tx AC Survial normal Patients")

ocm_lvwd_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvwd_echo, df = 4), data=ocm)
termplot(ocm_lvwd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvwd_echo", main = "lvwd_echo Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_lvwd_echo <- coxph(Surv(t,ac_event==1)~  pspline(lvwd_echo, df = 4), data=resp)
termplot(resp_lvwd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lvwd_echo", main = "lvwd_echo Post-Tx AC Survial resp Patients")


# IVSD_ECHO #
all_ivsd_echo <- coxph(Surv(t,ac_event==1)~  pspline(ivsd_echo, df = 4), data=cat)
termplot(all_ivsd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "ivsd_echo", main = "ivsd_echo Post-Tx AC Survial All Patients")

chf_ivsd_echo <- coxph(Surv(t,ac_event==1)~  pspline(ivsd_echo, df = 4), data=chf)
termplot(chf_ivsd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "ivsd_echo", main = "ivsd_echo Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_ivsd_echo <- coxph(Surv(t,ac_event==1)~  pspline(ivsd_echo, df = 5), data=normal)
termplot(normal_ivsd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "ivsd_echo", main = "ivsd_echo Post-Tx AC Survial normal Patients")

ocm_ivsd_echo <- coxph(Surv(t,ac_event==1)~  pspline(ivsd_echo, df = 4), data=ocm)
termplot(ocm_ivsd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "ivsd_echo", main = "ivsd_echo Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_ivsd_echo <- coxph(Surv(t,ac_event==1)~  pspline(ivsd_echo, df = 4), data=resp)
termplot(resp_ivsd_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "ivsd_echo", main = "ivsd_echo Post-Tx AC Survial resp Patients")



# LAMM_ECHO #
all_lamm_echo <- coxph(Surv(t,ac_event==1)~  pspline(lamm_echo, df = 4), data=cat)
termplot(all_lamm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lamm_echo", main = "lamm_echo Post-Tx AC Survial All Patients")

chf_lamm_echo <- coxph(Surv(t,ac_event==1)~  pspline(lamm_echo, df = 4), data=chf)
termplot(chf_lamm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lamm_echo", main = "lamm_echo Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_lamm_echo <- coxph(Surv(t,ac_event==1)~  pspline(lamm_echo, df = 5), data=normal)
termplot(normal_lamm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lamm_echo", main = "lamm_echo Post-Tx AC Survial normal Patients")

ocm_lamm_echo <- coxph(Surv(t,ac_event==1)~  pspline(lamm_echo, df = 4), data=ocm)
termplot(ocm_lamm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lamm_echo", main = "lamm_echo Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_lamm_echo <- coxph(Surv(t,ac_event==1)~  pspline(lamm_echo, df = 4), data=resp)
termplot(resp_lamm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "lamm_echo", main = "lamm_echo Post-Tx AC Survial resp Patients")



# AOMM_ECHO #
all_aomm_echo <- coxph(Surv(t,ac_event==1)~  pspline(aomm_echo, df = 4), data=cat)
termplot(all_aomm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "aomm_echo", main = "aomm_echo Post-Tx AC Survial All Patients")

chf_aomm_echo <- coxph(Surv(t,ac_event==1)~  pspline(aomm_echo, df = 4), data=chf)
termplot(chf_aomm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "aomm_echo", main = "aomm_echo Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_aomm_echo <- coxph(Surv(t,ac_event==1)~  pspline(aomm_echo, df = 5), data=normal)
termplot(normal_aomm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "aomm_echo", main = "aomm_echo Post-Tx AC Survial normal Patients")

ocm_aomm_echo <- coxph(Surv(t,ac_event==1)~  pspline(aomm_echo, df = 4), data=ocm)
termplot(ocm_aomm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "aomm_echo", main = "aomm_echo Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_aomm_echo <- coxph(Surv(t,ac_event==1)~  pspline(aomm_echo, df = 4), data=resp)
termplot(resp_aomm_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "aomm_echo", main = "aomm_echo Post-Tx AC Survial resp Patients")

# LAAO_ECHO # 
all_laao_echo <- coxph(Surv(t,ac_event==1)~  pspline(laao_echo, df = 4), data=cat)
termplot(all_laao_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "laao_echo", main = "laao_echo Post-Tx AC Survial All Patients")

chf_laao_echo <- coxph(Surv(t,ac_event==1)~  pspline(laao_echo, df = 4), data=chf)
termplot(chf_laao_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "laao_echo", main = "laao_echo Post-Tx AC Survial chf Patients")

# Not estimatable #
normal_laao_echo <- coxph(Surv(t,ac_event==1)~  pspline(laao_echo, df = 5), data=normal)
termplot(normal_laao_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "laao_echo", main = "laao_echo Post-Tx AC Survial normal Patients")

ocm_laao_echo <- coxph(Surv(t,ac_event==1)~  pspline(laao_echo, df = 4), data=ocm)
termplot(ocm_laao_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "laao_echo", main = "laao_echo Post-Tx AC Survial ocm Patients")

# Not estimatable #
resp_laao_echo <- coxph(Surv(t,ac_event==1)~  pspline(laao_echo, df = 4), data=resp)
termplot(resp_laao_echo, term = 1,   se = TRUE, ylab = "Log Hazard", rug=TRUE,  
         xlab = "laao_echo", main = "laao_echo Post-Tx AC Survial resp Patients")

 
WT_KG AGE_Y Troponin_I T4 CRT BUN
LVC_ECHO LVS_ECHO FS_ECHO LVWD_ECHO IVSD_ECHO LAMM_ECHO AOMM_ECHO LAAO_ECHO




(1)  The termplot for coxph shows the log hazard relative to the average risk, not relative to the baseline hazard
(2)	Yes.  If you want several PNG files, just open and close several PNG devices:
  
  png("plot1.png")
termplot(.)
dev.off()
png("plot2.png")
termplot(.)
dev.off()
png("plot3.png")
termplot(.)
dev.off()

(3)	If you want several plots in a PDF, just open a PDF device and plot:
  
  pdf("plots.pdf")
termplot(.)
termplot(.)
termplot(.)
dev.off()






locator (1)
abline h=0
abline v = 50 or whatever number