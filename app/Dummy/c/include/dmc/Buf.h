// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Strings constructor.

#ifndef DMC_BUF_H
  #define DMC_BUF_H

///
typedef struct buf_Buf Buf;

/// buf_new initializes a 'buf' with size 150
Buf *buf_new(void);

/// buf_bf_new initializes a 'buf'
Buf *buf_bf_new(int buffer_size);

///
void buf_free(Buf *this);

/// buf_len returns the length of the enveloped string
int buf_len(Buf *this);

/// buf_str returns the string wrapped. Return is intended to be not modified.
char *buf_str(Buf *this);

/// buf_add_buf adds 'length bytes of 'data' to 'buf'.<br>
/// 'length' must be less or equals to 'strlen(data)'.<br>
/// It is not necessary that 'data' be a null-terminated string, but it must
/// no have characters \0
void buf_add_buf(Buf *this, const char *data, int length);

/// buf_add adds 'data' to 'buf'.
void buf_add(Buf *this, const char *data);

/// buf_cadd adds a character
void buf_cadd(Buf *this, char data);

/// buf_to_str_new returns a copy of the enveloped string
char *buf_to_str_new(Buf *this);

/// buf_reset resets buffer (but does not reduce its size)
void buf_reset(Buf *this);

#endif
