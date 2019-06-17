#include <modsim.h>
#include <windows.h>

MS_INTEGER getXres(){ 
  MS_INTEGER x = GetSystemMetrics(SM_CXSCREEN); 
   return x;
   }
   
MS_INTEGER getYres(){ 
  MS_INTEGER y = GetSystemMetrics(SM_CYSCREEN); 
   return y;
   }




 
