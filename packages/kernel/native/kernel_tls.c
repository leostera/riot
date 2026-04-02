/* TLS implementation using OpenSSL BIO pairs for transport abstraction */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#if defined(__APPLE__) || defined(__linux__)
#include <openssl/ssl.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/x509.h>

/* TLS engine with BIO pairs - decouples SSL from file descriptors */
typedef struct {
    SSL_CTX *ctx;
    SSL *ssl;
    BIO *internal_bio;  /* SSL reads/writes here */
    BIO *network_bio;   /* We pump data through here */
    int is_server;
    int handshake_done;
} tls_engine_t;

static const char *ssl_error_name(int err) {
    switch (err) {
        case SSL_ERROR_NONE:
            return "SSL_ERROR_NONE";
        case SSL_ERROR_SSL:
            return "SSL_ERROR_SSL";
        case SSL_ERROR_WANT_READ:
            return "SSL_ERROR_WANT_READ";
        case SSL_ERROR_WANT_WRITE:
            return "SSL_ERROR_WANT_WRITE";
        case SSL_ERROR_WANT_X509_LOOKUP:
            return "SSL_ERROR_WANT_X509_LOOKUP";
        case SSL_ERROR_SYSCALL:
            return "SSL_ERROR_SYSCALL";
        case SSL_ERROR_ZERO_RETURN:
            return "SSL_ERROR_ZERO_RETURN";
        case SSL_ERROR_WANT_CONNECT:
            return "SSL_ERROR_WANT_CONNECT";
        case SSL_ERROR_WANT_ACCEPT:
            return "SSL_ERROR_WANT_ACCEPT";
#ifdef SSL_ERROR_WANT_ASYNC
        case SSL_ERROR_WANT_ASYNC:
            return "SSL_ERROR_WANT_ASYNC";
#endif
#ifdef SSL_ERROR_WANT_ASYNC_JOB
        case SSL_ERROR_WANT_ASYNC_JOB:
            return "SSL_ERROR_WANT_ASYNC_JOB";
#endif
#ifdef SSL_ERROR_WANT_CLIENT_HELLO_CB
        case SSL_ERROR_WANT_CLIENT_HELLO_CB:
            return "SSL_ERROR_WANT_CLIENT_HELLO_CB";
#endif
        default:
            return "SSL_ERROR_UNKNOWN";
    }
}

__attribute__((noreturn))
static void fail_ssl_operation(const char *operation, int ssl_err) {
    char message[512];
    unsigned long openssl_err = ERR_peek_last_error();

    if (openssl_err != 0) {
        char openssl_message[256];
        ERR_error_string_n(openssl_err, openssl_message, sizeof(openssl_message));
        snprintf(
            message,
            sizeof(message),
            "%s failed (%s): %s",
            operation,
            ssl_error_name(ssl_err),
            openssl_message
        );
    } else if (ssl_err == SSL_ERROR_SYSCALL && errno != 0) {
        snprintf(
            message,
            sizeof(message),
            "%s failed (%s): errno=%d (%s)",
            operation,
            ssl_error_name(ssl_err),
            errno,
            strerror(errno)
        );
    } else {
        snprintf(
            message,
            sizeof(message),
            "%s failed (%s)",
            operation,
            ssl_error_name(ssl_err)
        );
    }

    caml_failwith(message);
}

/* Custom block operations for garbage collection */
static void tls_engine_finalize(value v) {
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v);
    if (engine->ssl) SSL_free(engine->ssl);
    if (engine->ctx) SSL_CTX_free(engine->ctx);
    /* BIOs are freed by SSL_free */
}

static struct custom_operations tls_engine_ops = {
    "riot.kernel.tls_engine",
    tls_engine_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

/* Initialize OpenSSL library */
CAMLprim value kernel_tls_init(value unit) {
    CAMLparam1(unit);
    SSL_load_error_strings();
    SSL_library_init();
    OpenSSL_add_all_algorithms();
    CAMLreturn(Val_unit);
}

/* Check if OpenSSL is available */
CAMLprim value kernel_tls_is_available(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_bool(1));
}

/* Get OpenSSL version */
CAMLprim value kernel_tls_version(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);
    const char *version = OpenSSL_version(OPENSSL_VERSION);
    result = caml_copy_string(version);
    CAMLreturn(result);
}

/* Create client TLS engine with BIO pairs */
CAMLprim value kernel_tls_create_client_engine(value hostname_val) {
    CAMLparam1(hostname_val);
    CAMLlocal1(v_engine);
    
    const char *hostname = String_val(hostname_val);
    
    /* Create SSL context */
    SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
    if (!ctx) {
        caml_failwith("Failed to create SSL context");
    }
    
    /* Set modern TLS versions only (1.2+) */
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    
    /* Enable certificate verification */
    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
    SSL_CTX_set_default_verify_paths(ctx);  /* Use system cert store */
    
    /* Create SSL object */
    SSL *ssl = SSL_new(ctx);
    if (!ssl) {
        SSL_CTX_free(ctx);
        caml_failwith("Failed to create SSL object");
    }
    
    SSL_set_connect_state(ssl);  /* Client mode */
    SSL_set_tlsext_host_name(ssl, hostname);  /* SNI */
    
    /* Create BIO pair - this is the key to transport abstraction! */
    BIO *internal_bio, *network_bio;
    if (BIO_new_bio_pair(&internal_bio, 0, &network_bio, 0) != 1) {
        SSL_free(ssl);
        SSL_CTX_free(ctx);
        caml_failwith("Failed to create BIO pair");
    }
    
    /* Connect SSL to internal BIO */
    SSL_set_bio(ssl, internal_bio, internal_bio);
    
    /* Allocate custom block for engine */
    v_engine = caml_alloc_custom(&tls_engine_ops, sizeof(tls_engine_t), 0, 1);
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v_engine);
    engine->ctx = ctx;
    engine->ssl = ssl;
    engine->internal_bio = internal_bio;
    engine->network_bio = network_bio;
    engine->is_server = 0;
    engine->handshake_done = 0;
    
    CAMLreturn(v_engine);
}

/* Create server TLS engine with BIO pairs */
CAMLprim value kernel_tls_create_server_engine(value cert_file_val, value key_file_val) {
    CAMLparam2(cert_file_val, key_file_val);
    CAMLlocal1(v_engine);
    
    const char *cert_file = String_val(cert_file_val);
    const char *key_file = String_val(key_file_val);
    
    /* Create SSL context */
    SSL_CTX *ctx = SSL_CTX_new(TLS_server_method());
    if (!ctx) {
        caml_failwith("Failed to create SSL context");
    }
    
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    
    /* Load server certificate and key */
    if (SSL_CTX_use_certificate_file(ctx, cert_file, SSL_FILETYPE_PEM) != 1) {
        SSL_CTX_free(ctx);
        caml_failwith("Failed to load certificate file");
    }
    
    if (SSL_CTX_use_PrivateKey_file(ctx, key_file, SSL_FILETYPE_PEM) != 1) {
        SSL_CTX_free(ctx);
        caml_failwith("Failed to load private key file");
    }
    
    /* Create SSL object */
    SSL *ssl = SSL_new(ctx);
    if (!ssl) {
        SSL_CTX_free(ctx);
        caml_failwith("Failed to create SSL object");
    }
    
    SSL_set_accept_state(ssl);  /* Server mode */
    
    /* Create BIO pair */
    BIO *internal_bio, *network_bio;
    if (BIO_new_bio_pair(&internal_bio, 0, &network_bio, 0) != 1) {
        SSL_free(ssl);
        SSL_CTX_free(ctx);
        caml_failwith("Failed to create BIO pair");
    }
    
    SSL_set_bio(ssl, internal_bio, internal_bio);
    
    /* Allocate custom block */
    v_engine = caml_alloc_custom(&tls_engine_ops, sizeof(tls_engine_t), 0, 1);
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v_engine);
    engine->ctx = ctx;
    engine->ssl = ssl;
    engine->internal_bio = internal_bio;
    engine->network_bio = network_bio;
    engine->is_server = 1;
    engine->handshake_done = 0;
    
    CAMLreturn(v_engine);
}

/* Pump encrypted data FROM network INTO TLS engine */
CAMLprim value kernel_tls_pump_encrypted_in(value v_engine, value buf_val, 
                                             value pos_val, value len_val) {
    CAMLparam4(v_engine, buf_val, pos_val, len_val);
    
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v_engine);
    char *buf = (char*)Bytes_val(buf_val);
    int pos = Int_val(pos_val);
    int len = Int_val(len_val);
    
    /* Write encrypted data to network BIO (never blocks - memory only) */
    int written = BIO_write(engine->network_bio, buf + pos, len);
    
    CAMLreturn(Val_int(written));
}

/* Read decrypted application data FROM TLS engine
   Returns: n > 0 = bytes read, 0 = EOF, -1 = need network read, -2 = need network write */
CAMLprim value kernel_tls_read_decrypted(value v_engine, value buf_val,
                                          value pos_val, value len_val) {
    CAMLparam4(v_engine, buf_val, pos_val, len_val);
    
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v_engine);
    char *buf = (char*)Bytes_val(buf_val);
    int pos = Int_val(pos_val);
    int len = Int_val(len_val);
    
    /* Try to read decrypted data from SSL (never blocks - BIO pair) */
    errno = 0;
    ERR_clear_error();
    int bytes_read = SSL_read(engine->ssl, buf + pos, len);
    
    if (bytes_read > 0) {
        engine->handshake_done = 1;
        CAMLreturn(Val_int(bytes_read));
    }
    
    int err = SSL_get_error(engine->ssl, bytes_read);
    switch (err) {
        case SSL_ERROR_WANT_READ:
            /* SSL needs more encrypted data from network */
            CAMLreturn(Val_int(-1));
        case SSL_ERROR_WANT_WRITE:
            /* SSL needs to send encrypted data to network */
            CAMLreturn(Val_int(-2));
        case SSL_ERROR_ZERO_RETURN:
            /* Clean TLS shutdown */
            CAMLreturn(Val_int(0));
        case SSL_ERROR_SYSCALL:
        case SSL_ERROR_SSL:
            fail_ssl_operation("SSL_read", err);
        default:
            fail_ssl_operation("SSL_read", err);
    }
}

/* Write plaintext TO TLS engine (gets encrypted)
   Returns: n > 0 = bytes written, -1 = need network read, -2 = need network write */
CAMLprim value kernel_tls_write_plaintext(value v_engine, value buf_val,
                                           value pos_val, value len_val) {
    CAMLparam4(v_engine, buf_val, pos_val, len_val);
    
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v_engine);
    char *buf = (char*)Bytes_val(buf_val);
    int pos = Int_val(pos_val);
    int len = Int_val(len_val);
    
    /* Write plaintext to SSL (gets encrypted internally, never blocks) */
    errno = 0;
    ERR_clear_error();
    int bytes_written = SSL_write(engine->ssl, buf + pos, len);
    
    if (bytes_written > 0) {
        engine->handshake_done = 1;
        CAMLreturn(Val_int(bytes_written));
    }
    
    int err = SSL_get_error(engine->ssl, bytes_written);
    switch (err) {
        case SSL_ERROR_WANT_READ:
            CAMLreturn(Val_int(-1));
        case SSL_ERROR_WANT_WRITE:
            CAMLreturn(Val_int(-2));
        case SSL_ERROR_SYSCALL:
        case SSL_ERROR_SSL:
            fail_ssl_operation("SSL_write", err);
        default:
            fail_ssl_operation("SSL_write", err);
    }
}

/* Read encrypted data FROM TLS engine (to send to network) */
CAMLprim value kernel_tls_read_encrypted_out(value v_engine, value buf_val) {
    CAMLparam2(v_engine, buf_val);
    
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v_engine);
    char *buf = (char*)Bytes_val(buf_val);
    int buf_len = caml_string_length(buf_val);
    
    /* Read encrypted data from network BIO (never blocks) */
    int pending = BIO_ctrl_pending(engine->network_bio);
    if (pending == 0) {
        CAMLreturn(Val_int(0));
    }
    
    int to_read = (pending < buf_len) ? pending : buf_len;
    int bytes_read = BIO_read(engine->network_bio, buf, to_read);
    
    CAMLreturn(Val_int(bytes_read >= 0 ? bytes_read : 0));
}

/* Check if handshake is complete */
CAMLprim value kernel_tls_handshake_complete(value v_engine) {
    CAMLparam1(v_engine);
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v_engine);
    CAMLreturn(Val_bool(engine->handshake_done && SSL_is_init_finished(engine->ssl)));
}

/* Get negotiated ALPN protocol */
CAMLprim value kernel_tls_alpn_protocol(value v_engine) {
    CAMLparam1(v_engine);
    CAMLlocal1(result);
    
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v_engine);
    const unsigned char *data;
    unsigned int len;
    
    SSL_get0_alpn_selected(engine->ssl, &data, &len);
    
    if (len > 0) {
        result = caml_alloc(1, 0);  /* Some */
        Store_field(result, 0, caml_alloc_initialized_string(len, (const char*)data));
    } else {
        result = Val_int(0);  /* None */
    }
    
    CAMLreturn(result);
}

/* Explicitly trigger TLS handshake
   Returns: 0 = continue, -1 = need network read, -2 = need network write */
CAMLprim value kernel_tls_do_handshake(value v_engine) {
    CAMLparam1(v_engine);
    
    tls_engine_t *engine = (tls_engine_t*)Data_custom_val(v_engine);
    
    errno = 0;
    ERR_clear_error();
    int result = SSL_do_handshake(engine->ssl);
    
    if (result == 1) {
        /* Handshake complete */
        engine->handshake_done = 1;
        CAMLreturn(Val_int(0));
    }
    
    int err = SSL_get_error(engine->ssl, result);
    switch (err) {
        case SSL_ERROR_WANT_READ:
            CAMLreturn(Val_int(-1));
        case SSL_ERROR_WANT_WRITE:
            CAMLreturn(Val_int(-2));
        default:
            fail_ssl_operation("SSL_do_handshake", err);
    }
}

#else
/* Non-macOS platforms - stub implementations for now */

CAMLprim value kernel_tls_init(value unit) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_is_available(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_bool(0));
}

CAMLprim value kernel_tls_version(value unit) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_create_client_engine(value hostname_val) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_create_server_engine(value cert_file_val, value key_file_val) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_pump_encrypted_in(value v_engine, value buf_val, 
                                             value pos_val, value len_val) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_read_decrypted(value v_engine, value buf_val,
                                          value pos_val, value len_val) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_write_plaintext(value v_engine, value buf_val,
                                           value pos_val, value len_val) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_read_encrypted_out(value v_engine, value buf_val) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_handshake_complete(value v_engine) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_alpn_protocol(value v_engine) {
    caml_failwith("TLS not available on this platform");
}

CAMLprim value kernel_tls_do_handshake(value v_engine) {
    caml_failwith("TLS not available on this platform");
}

#endif
