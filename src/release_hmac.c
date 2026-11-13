#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#ifdef DSOMOP_HAVE_OPENSSL
#define OPENSSL_SUPPRESS_DEPRECATED
#include <openssl/hmac.h>

/* Request-local reuse of OpenSSL's keyed context. The HMAC bytes are exactly
 * those of openssl::sha256(text, key); no key, token or answer is cached. */
typedef struct {
    SEXP text;
    SEXP key;
    HMAC_CTX *ctx;
    int chunks;
} HmacRequest;

static void hmac_cleanup(void *data) {
    HmacRequest *request = (HmacRequest *) data;
    if (request->ctx) HMAC_CTX_free(request->ctx);
}

static SEXP hmac_run(void *data) {
    HmacRequest *request = (HmacRequest *) data;
    R_xlen_t length = XLENGTH(request->text);
    R_xlen_t count = request->chunks ? length / 10000 + (length % 10000 != 0) : length;
    SEXP out = PROTECT(Rf_allocVector(STRSXP, count));
    request->ctx = HMAC_CTX_new();
    if (!request->ctx || !HMAC_Init_ex(request->ctx, RAW(request->key), 32,
                                      EVP_sha256(), NULL))
        Rf_error("Protected person hashing is unavailable.");
    const char *digits = "0123456789abcdef";
    for (R_xlen_t i = 0; i < count; i++) {
        if (i % 1000 == 0) R_CheckUserInterrupt();
        unsigned char hash[EVP_MAX_MD_SIZE];
        unsigned int size = 0;
        char hex[65];
        if (!HMAC_Init_ex(request->ctx, NULL, 0, NULL, NULL))
            Rf_error("Protected contribution hashing is unavailable.");
        R_xlen_t start = request->chunks ? i * 10000 : i;
        R_xlen_t end = request->chunks ? start + 10000 : start + 1;
        if (end > length) end = length;
        if (request->chunks) {
            char header[64];
            int n = snprintf(header, sizeof(header), "dsomop-contribution-chunk-v2\n%d",
                             (int) (end - start));
            if (n < 0 || n >= (int) sizeof(header) ||
                !HMAC_Update(request->ctx, (const unsigned char *) header, n))
                Rf_error("Protected contribution hashing is unavailable.");
        }
        for (R_xlen_t j = start; j < end; j++) {
            if (j % 1000 == 0) R_CheckUserInterrupt();
            SEXP item = STRING_ELT(request->text, j);
            if (item == NA_STRING) Rf_error("Protected person hashing needs complete text.");
            const char *bytes = Rf_translateCharUTF8(item);
            if ((request->chunks && !HMAC_Update(request->ctx, (const unsigned char *) "\n", 1)) ||
                !HMAC_Update(request->ctx, (const unsigned char *) bytes, strlen(bytes)))
                Rf_error("Protected contribution hashing is unavailable.");
        }
        if (!HMAC_Final(request->ctx, hash, &size) || size != 32)
            Rf_error("Protected contribution hashing is unavailable.");
        for (int j = 0; j < 32; j++) {
            hex[2*j] = digits[hash[j] >> 4];
            hex[2*j+1] = digits[hash[j] & 15];
        }
        hex[64] = '\0';
        SET_STRING_ELT(out, i, Rf_mkCharLenCE(hex, 64, CE_UTF8));
    }
    UNPROTECT(1);
    return out;
}
#endif

static SEXP hmac_dispatch(SEXP text, SEXP key, int chunks) {
    if (TYPEOF(text) != STRSXP || TYPEOF(key) != RAWSXP || XLENGTH(key) != 32)
        Rf_error("Invalid protected person hashing input.");
#ifdef DSOMOP_HAVE_OPENSSL
    HmacRequest request = {text, key, NULL, chunks};
    return R_ExecWithCleanup(hmac_run, &request, hmac_cleanup, &request);
#else
    /* Builds without libcrypto headers use the existing R openssl path. */
    return R_NilValue;
#endif
}

static SEXP dsomop_person_hmac(SEXP text, SEXP key) {
    return hmac_dispatch(text, key, 0);
}

static SEXP dsomop_fingerprint_chunks(SEXP records, SEXP key) {
    return hmac_dispatch(records, key, 1);
}

static const R_CallMethodDef call_methods[] = {
    {"dsomop_person_hmac", (DL_FUNC) &dsomop_person_hmac, 2},
    {"dsomop_fingerprint_chunks", (DL_FUNC) &dsomop_fingerprint_chunks, 2},
    {NULL, NULL, 0}
};

void attribute_visible R_init_dsOMOP(DllInfo *dll) {
    R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
