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

{$I ../../Zeos.inc}
