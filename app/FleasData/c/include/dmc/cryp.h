// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Cryptographic utilities

#ifndef DMC_CRYP_H
  #define DMC_CRYP_H

/// Generates a b64 random key of a length 'lg'
char *cryp_genk_new (int lg);

/// Codified 'k' in irreversible way, using 'lg' b64 digits.
///   k   : String to codify
///   lg  : Length of result
void cryp_key (char **k, int lg);

/// Encodes 's' with key 'k'.
///   s   : Message to encode
///   k   : Key for encoding
void cryp_cryp (char **s, const char *k);

/// Decodes 'b64' using key 'k'. 'b64' was codified with cryp()
///   b64: Text codified with cryp()
///   k   : Key for decoding
void cryp_decryp (char **b64, const char *k);

/// Encodes automatically 's' with a random key of 'nk' digits.
///   nK  : Number of digits for random key (1 to 64 both inclusive)
///   s   : Text for enconding
void cryp_auto_cryp (char **s, int nk);

/// Decodes a text codified with autoCryp()
///   b64: Codified text
void cryp_auto_decryp (char **b64);

/// Encodes 's' whith key 'k' and an autoKey of length 'nK'
///   k   : Key for encoding
///   nk  : Digits to generate autoKey (1 to 40 both inclusive)
///   s   : Message to encode
void cryp_encode (char **s, int nk, const char *k);

/// Decodes a string codified with encode()
///   b64: Message encoded with encode()
///   k   : Key for encoding
void cryp_decode (char **b64, const char *k);

#endif
