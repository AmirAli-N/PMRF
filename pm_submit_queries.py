#import sys
import os
os.chdir("//ahmct-065/teams/PMRF/Amir/Codes")
import pm_odom_query
import pandas as pd
import numpy as np
import math
from time import sleep
from socket import gethostbyname, gaierror

##Travis function to query from the pm_odom_query file
def odom_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, alignment):
    odoms = pm_odom_query.getodomfrompm_caltrans(cty, rtnum, rtsfx, pmpfx, pmval, alignment)
    if (len(odoms) != 1):
        return None
    return odoms[0]

## Funtion to convert numpy types to python standard types
#def convert_to_python_types(obj):
#    if isinstance(obj, np.generic):
#        return np.asscalar(obj)    

#set of postmile prefix, route suffix, and postmile suffix
pmpfx_lst=['R', 'M', 'C', 'D', 'G', 'H', 'L', 'N', 'S', 'T']
rtsfx_lst=['U', 'S']
align_lst=['R', 'L']
os.chdir("//ahmct-065/teams/PMRF/Amir/bin")
##################################################################################################################################################
##################################################################################################################################################
########################################################### LEMO_WorkOrder pm to odom ############################################################
##read the csv files
##NOTE: switch the reading files when converting pm to odom for the end mark
from_df=pd.read_csv("./fromPM.csv")
to_df=pd.read_csv("./toPM.csv")

##replace missing values with None
from_df=from_df.where((pd.notnull(from_df)), None)
to_df=to_df.where((pd.notnull(to_df)), None)

for i in range(0, from_df.shape[0]):
    for pmsfx in align_lst:
        cty=from_df.at[i, 'County']
        if (cty is None):
            continue
        rtnum=from_df.at[i, 'rID']
        if (rtnum is None):
            continue
        else:
            rtnum=int(from_df.at[i, 'rID'])
        rtsfx=from_df.at[i, 'rSuffix']
        pmpfx=from_df.at[i, 'PMprefix']
        pmval=from_df.at[i, 'PM']
        if (pmval is None):
            continue
        else:
            pmval=round(float(from_df.at[i, 'PM']), 3)        
##submit with original data
        try:
            from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, pmsfx)
            sleep(0.05)
        except (gaierror, TimeoutError, ConnectionResetError, OSError):
            sleep(60)
            from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, pmsfx)
##if no match were returned with original data, submit with no route suffix, and no pm prefix
        if (math.isnan(from_df.at[i, pmsfx])):
            try:
                from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, None, None, pmval, pmsfx)
                sleep(0.05)
            except (gaierror, TimeoutError, ConnectionResetError, OSError):
                sleep(60)
                from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, None, None, pmval, pmsfx)
##if no match were returned, submit all permutations of pm prefix 
        if (math.isnan(from_df.at[i, pmsfx])):
            for pfx in pmpfx_lst:
                try:
                    from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pfx, pmval, pmsfx)
                    sleep(0.05)
                except (gaierror, TimeoutError, ConnectionResetError, OSError):
                    sleep(60)
                    from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pfx, pmval, pmsfx)
                if (math.isnan(from_df.at[i, pmsfx])):
                    continue
                else:
##exit on first match
                    break
##if no match were returned, it might be the case that the end or begin county are mistaken
        if (math.isnan(from_df.at[i, pmsfx])):
            if((from_df.at[i, 'County']!=to_df.at[i, 'County']) and (to_df.at[i, 'County'] is not None)):
                try:
                    from_df.at[i, pmsfx]=odom_from_pm(to_df.at[i, 'County'], rtnum, rtsfx, pmpfx, pmval, pmsfx)
                    sleep(0.05)
                except (gaierror, TimeoutError, ConnectionResetError, OSError):
                    sleep(60)
                    from_df.at[i, pmsfx]=odom_from_pm(to_df.at[i, 'County'], rtnum, rtsfx, pmpfx, pmval, pmsfx)

from_df.to_csv("LEMO-to.csv", sep=",")
##################################################################################################################################################
##################################################################################################################################################
################################################################# LCS pm to odom #################################################################
##read the csv files
##NOTE: change the order of reading when converting pm to odom for the end mark
from_df=pd.read_csv("./EndLane.csv")
to_df=pd.read_csv("./BeginLane.csv")

##county for 980 which is entirely in Alameda is missing
#to_df.loc[to_df['rID']==980, 'County']='ALA'

#replace missing values with None
from_df=from_df.where((pd.notnull(from_df)), None)
to_df=to_df.where((pd.notnull(to_df)), None)

for i in range(0, from_df.shape[0]):
    for pmsfx in align_lst:
        cty=from_df.at[i, 'County']
        if (cty is None):
            continue
        rtnum=from_df.at[i, 'rID']
        if (rtnum is None):
            continue
        else:
            rtnum=int(from_df.at[i, 'rID'])    
        rtsfx=None
        pmpfx=None
        pmval=from_df.at[i, 'PM']
        if (pmval is None):
            continue
        else:
            pmval=round(float(from_df.at[i, 'PM']), 3)
        #submit with original data
        try:
            from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, pmsfx)
            sleep(0.05)
        except (gaierror, TimeoutError, ConnectionResetError, OSError):
            sleep(60)
            from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, pmsfx)
        #if no match were returned, submit all permutations of pm prefix 
        if (math.isnan(from_df.at[i, pmsfx])):
            for pfx in pmpfx_lst:
                try:
                    from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pfx, pmval, pmsfx)
                    sleep(0.05)
                except (gaierror, TimeoutError, ConnectionResetError, OSError):
                    sleep(60)
                    from_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pfx, pmval, pmsfx)
                if (math.isnan(from_df.at[i, pmsfx])):
                    continue
                else:
                    #exit on first match
                    break
        #if no match were returned, it might be the case that the end or begin county are mistaken
        if (math.isnan(from_df.at[i, pmsfx])):
            if((from_df.at[i, 'County']!=to_df.at[i, 'County']) and (to_df.at[i, 'County'] is not None)):
                try:
                    from_df.at[i, pmsfx]=odom_from_pm(to_df.at[i, 'County'], rtnum, rtsfx, pmpfx, pmval, pmsfx)
                    sleep(0.05)
                except (gaierror, TimeoutError, ConnectionResetError, OSError):
                    sleep(60)
                    from_df.at[i, pmsfx]=odom_from_pm(to_df.at[i, 'County'], rtnum, rtsfx, pmpfx, pmval, pmsfx)

from_df.to_csv("LCS-to.csv", sep=",")
##################################################################################################################################################
##################################################################################################################################################
################################################################# AADT pm to odom ################################################################
##read the csv files
aadt_df=pd.read_csv("./pm_aadt.csv")

##replace missing values with None
aadt_df=aadt_df.where((pd.notnull(aadt_df)), None)

for i in range(0, aadt_df.shape[0]):
    for pmsfx in align_lst:
        cty=aadt_df.at[i, 'County']
        if (cty is None):
            continue
        rtnum=aadt_df.at[i, 'Route']
        if (rtnum is None):
            continue
        else:
            rtnum=int(aadt_df.at[i, 'Route'])
        rtsfx=aadt_df.at[i, 'rtsfx']
        pmpfx=aadt_df.at[i, 'pmpfx']
        pmval=aadt_df.at[i, 'Postmile']
        if (pmval is None):
            continue
        #submit with original data
        aadt_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, pmsfx)
        sleep(0.05)
        #if no match were returned, submit all permutations of pm prefix 
        if (math.isnan(aadt_df.at[i, pmsfx])):
            for pfx in pmpfx_lst:
                aadt_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pfx, pmval, pmsfx)
                sleep(0.05)
                if (math.isnan(aadt_df.at[i, pmsfx])):
                    continue
                else:
                    #exit on first match
                    break
aadt_df.to_csv("aadt_odom.csv", sep=",")
##################################################################################################################################################
##################################################################################################################################################
################################################################ Truck pm to odom ################################################################
##read the csv files
truck_df=pd.read_csv("./pm_truck.csv")

##replace missing values with None
truck_df=truck_df.where((pd.notnull(truck_df)), None)

for i in range(0, truck_df.shape[0]):
    for pmsfx in align_lst:
        cty=truck_df.at[i, 'County']
        if (cty is None):
            continue
        rtnum=truck_df.at[i, 'Route']
        if (rtnum is None):
            continue
        else:
            rtnum=int(truck_df.at[i, 'Route'])
        rtsfx=truck_df.at[i, 'rtsfx']
        pmpfx=truck_df.at[i, 'pmpfx']
        pmval=truck_df.at[i, 'Postmile']
        if (pmval is None):
            continue
        else:
            pmval=float(pmval)
        #submit with original data
        truck_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, pmsfx)
        sleep(0.05)
        #if no match were returned, submit all permutations of pm prefix 
        if (math.isnan(truck_df.at[i, pmsfx])):
            for pfx in pmpfx_lst:
                truck_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pfx, pmval, pmsfx)
                sleep(0.05)
                if (math.isnan(truck_df.at[i, pmsfx])):
                    continue
                else:
                    #exit on first match
                    break
truck_df.to_csv("truck_odom.csv", sep=",")

##################################################################################################################################################
##################################################################################################################################################
################################################################# chp pm to odom #################################################################
#read the csv file
os.chdir("//ahmct-065/teams/PMRF/Amir/bin")

chp_df=pd.read_csv("./chp.to.odom.csv")
chp_df['ROUTE_SUFFIX']=chp_df['ROUTE_SUFFIX'].where((chp_df['ROUTE_SUFFIX']!='-'), None)
chp_df['POSTMILE_PREFIX']=chp_df['POSTMILE_PREFIX'].where((chp_df['POSTMILE_PREFIX']!='-'), None)
chp_df=chp_df.where((pd.notnull(chp_df)), None)

for i in range(0, chp_df.shape[0]):
    for pmsfx in align_lst:
        cty=chp_df.at[i, 'CALTRANS_COUNTY']
        if (cty is None):
            continue
        rtnum=chp_df.at[i, 'STATE_ROUTE']
        if (rtnum is None):
            continue
        else:
            rtnum=int(chp_df.at[i, 'STATE_ROUTE'])
        rtsfx=chp_df.at[i, 'ROUTE_SUFFIX']
        pmpfx=chp_df.at[i, 'POSTMILE_PREFIX']
        pmval=chp_df.at[i, 'POSTMILE']
        if (pmval is None):
            continue
        else:
            pmval=float(pmval)
        #submit with original data
        chp_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pmpfx, pmval, pmsfx)
        sleep(0.05)
        #if no match were returned, submit all permutations of pm prefix 
        if (math.isnan(chp_df.at[i, pmsfx])):
            for pfx in pmpfx_lst:
                try:
                    chp_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pfx, pmval, pmsfx)
                    sleep(0.05)
                except (gaierror, TimeoutError, ConnectionResetError, OSError):
                    sleep(60.0)
                    chp_df.at[i, pmsfx]=odom_from_pm(cty, rtnum, rtsfx, pfx, pmval, pmsfx)
                if (math.isnan(chp_df.at[i, pmsfx])):
                    continue
                else:
                    #exit on first match
                    break
chp_df.to_csv("chp_odom-2013.csv", sep=",")
