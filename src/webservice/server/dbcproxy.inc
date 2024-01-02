// If enabled, the LDAP security module will be compiled. It requires Synapse
// (https://sourceforge.net/p/synalist/code/HEAD/tree/) to be in the unit search
// path. You can add it to the project options of the server.
{.$DEFINE ENABLE_LDAP_SECURITY}

// If enabled, the server can generate certificates for a trust on first use style
// authentication.
// If you want to compile this functionality, you will need the IKS branch of a
// modified synapse library from https://github.com/marsupilami79/synapse
{.$DEFINE ENABLE_TOFU_CERTIFICATES}

// On Linux this will change where the config file is read from and where
// certificates for TOFU are stored.
{.$DEFINE ENABLE_DEBUG_SETTINGS}

// If enabled, this will make the server register with the Windows DNS Service Discovery
// facility. This can enable other devices on the network to find the service.
// To compile this, you will need the MDNS / DNS-SD components from:
// https://github.com/marsupilami79/mdns4Delphi
{.$DEFINE ENABLE_DNSSD}


{$I ../../Zeos.inc}

// Do not change anything below this line if you are not sure, what you are doing.

{$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
    {$DEFINE WINDOWS}
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(ENABLE_DNSSD) AND DEFINED(WINDOWS)}
  {$DEFINE WITH_DNSSD}
{$IFEND}

