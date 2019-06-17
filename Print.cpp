#include <modsim.h>
#include <windows.h>
#include <osmod.h>
#include <string.h>
#include <shlobj.h>
#include <shellapi.h>



#define MAX_SIZE 100

void PrintRBP( void )
{
    char   sProgDir[MAX_SIZE];
    char   sWorkDir[MAX_SIZE];
    // int    iret;
    TCHAR szPath[_MAX_PATH];
    MS_STRING sFilename = "\\Raptor7\\RapPrin70.exe";
    
      
  SHGetSpecialFolderPath( NULL, szPath, CSIDL_APPDATA, FALSE );

    if( strlen( szPath ) < (MAX_SIZE - strlen( sFilename ) - 1))
    {
        memcpy( sProgDir, szPath, strlen(szPath) + 1 );
    memcpy (sWorkDir, szPath, strlen(szPath) + 1 );
    
        strcat( sProgDir, sFilename );
        strcat( sWorkDir, "\\raptor7" );

//    the following doesn't wait for RapPrin70.exe to finish... use ShellExecuteEx instead
//    iret = (int) ShellExecute( NULL, "open", sProgDir, "/p fromRBD.rbp", sWorkDir, SW_SHOWNORMAL);      

        SHELLEXECUTEINFO ShExecInfo = {0};
        ShExecInfo.cbSize = sizeof(SHELLEXECUTEINFO);
        ShExecInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
        ShExecInfo.hwnd = NULL;
        ShExecInfo.lpVerb = "open";
        ShExecInfo.lpFile = sProgDir;       
        ShExecInfo.lpParameters = "/p fromRBD.rbp"; 
        ShExecInfo.lpDirectory = sWorkDir;
        ShExecInfo.nShow = NULL;
        ShExecInfo.hInstApp = NULL; 
        bool res = ShellExecuteEx(&ShExecInfo);
        if( res )
        {
            WaitForSingleObject(ShExecInfo.hProcess,INFINITE);
            CloseHandle( ShExecInfo.hProcess );
        }
    }
}

void ConvertRBP( void )
{

    char   sProgDir[MAX_SIZE];
    char   sWorkDir[MAX_SIZE];
    TCHAR szPath[_MAX_PATH];
    MS_STRING sFilename = "\\Raptor7\\RapPrin70.exe";

  SHGetSpecialFolderPath( NULL, szPath, CSIDL_APPDATA, FALSE );

    if( strlen( szPath ) < (MAX_SIZE - strlen( sFilename ) - 1))
    {
        memcpy( sProgDir, szPath, strlen(szPath) + 1 );
    memcpy (sWorkDir, szPath, strlen(szPath) + 1 );
    
        strcat( sProgDir, sFilename );
        strcat( sWorkDir, "\\Raptor7" );
        
 //    the following doesn't wait for RapPrin70.exe to finish... use ShellExecuteEx instead
     ShellExecute( NULL, "open", sProgDir, "fromRBD.rbp", sWorkDir, SW_SHOWNORMAL);     //   {cmc 10/9/08}
      
    
/*    {cmc 10/9/08}
   SHELLEXECUTEINFO ShExecInfo = {0};
        ShExecInfo.cbSize = sizeof(SHELLEXECUTEINFO);
        ShExecInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
        ShExecInfo.hwnd = NULL;
        ShExecInfo.lpVerb = "open";
        ShExecInfo.lpFile = sProgDir;       
        ShExecInfo.lpParameters = "fromRBD.rbp";    
        ShExecInfo.lpDirectory = sWorkDir;
        ShExecInfo.nShow = NULL;
        ShExecInfo.hInstApp = NULL; 
       bool res = ShellExecuteEx(&ShExecInfo);
        if( res )
        {
            WaitForSingleObject(ShExecInfo.hProcess,INFINITE);
            CloseHandle( ShExecInfo.hProcess );
        }
 */   //{cmc 10/9/08} 
    }
 
}


