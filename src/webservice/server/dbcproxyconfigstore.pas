unit DbcProxyConfigStore;

{$I dbcproxy.inc}

interface

uses
  Classes, SysUtils;


  type
    IZDbcProxyKeyValueStore = interface;

    IZDbcProxyConfigStore = interface(IUnknown)
      ['{3B52C9F5-00AE-443A-9632-98A6041BEAA4}']
      function GetSecurityConfig(const Name: String): IZDbcProxyKeyValueStore;
      function GetDatbaseConfig(const Name: String): IZDbcProxyKeyValueStore;
      // Constructs the DBC layer URL
      function ConstructUrl(ConfigName, UserName, Password: String; CheckSecurity: Boolean = True): String; virtual; abstract;

      function GetUseSSL: Boolean;
      property UseSSL: Boolean read GetUseSSL;
      function GetListeningPort: Word;
      property ListeningPort: Word read GetListeningPort;
      function GetIPAddress: String;
      property IPAddress: String read GetIPAddress;
      function GetConnectionIdleTimeout: Cardinal;
      property ConnectionIdleTimeout: Cardinal read GetConnectionIdleTimeout;
      function GetEnableThreading: Boolean;
      property EnableThreading: Boolean read GetEnableThreading;
      function GetLogFile: String;
      property LogFile: String read GetLogFile;
      {$IFDEF ENABLE_TOFU_CERTIFICATES}
      // Wether to use TOFU style SSL
      function GetUseTofuSSL: Boolean;
      property UseTofuSSL: Boolean read GetUseTofuSSL;
      {$ENDIF}
      // The SSL host name to use.
      function GetHostName: String;
      property HostName: String read GetHostName;
      // The certificate file to use for standard fashioned SSL
      function GetCertificateFile: String;
      property CertificateFile: String read GetCertificateFile;
      // The key file to use for standard fashioned SSL
      function GetKeyFile: String;
      property KeyFile: String read GetKeyFile;
      // The key file password to use for standard fashioned SSL
      function GetKeyPassword: String;
      // The key file password to use for standard fashioned SSL
      property KeyPassword: String read GetKeyPassword;
      procedure LoadBaseConfig;
      procedure LoadConnectionConfig;
    end;

    IZDbcProxyKeyValueStore = interface(IUnknown)
      ['{06C13806-CF23-4BA5-B3E3-93BDDFE955B1}']
      function ReadString(const Key, DefaultValue: String): String;
      function ReadInteger(const Key: String; DefaultValue: Integer): Integer;
      function ReadBool(const Key: String; DefaultValue: Boolean): Boolean;
      function GetName: String;
      function GetConfigStore: IZDbcProxyConfigStore;
    end;

implementation

end.

