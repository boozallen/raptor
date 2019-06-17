#include <modsim.h>
#include <windows.h>
#include <shlobj.h>

extern void Display_getUserFolderPathAux(MS_CHAR* astr);
extern void Display_getUserMyDocsPathAux(MS_CHAR* astr);



MS_INTEGER getXres(){ 
  MS_INTEGER x = GetSystemMetrics(SM_CXSCREEN); 
   return x;
   }
   
MS_INTEGER getYres(){ 
  MS_INTEGER y = GetSystemMetrics(SM_CYSCREEN); 
   return y;
   }


void getUserFolderPath( void )
{ 
  TCHAR szPath[_MAX_PATH];
  
  SHGetSpecialFolderPath( NULL, szPath, CSIDL_APPDATA, FALSE );
  Display_getUserFolderPathAux((MS_CHAR*)szPath); // call MODSIM PROCEDURE to convert and store pathname
}


void getUserMyDocsPath( void )
{ 
  TCHAR szPath[_MAX_PATH];
  
  SHGetSpecialFolderPath( NULL, szPath, CSIDL_PERSONAL, FALSE );
  Display_getUserMyDocsPathAux((MS_CHAR*)szPath); // call MODSIM PROCEDURE to convert and store pathname
}


 
