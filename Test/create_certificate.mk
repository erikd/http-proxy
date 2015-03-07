#!/usr/bin/make -f

# Generate SSL certificate and keys for the tests.
# Partially taken from:
#     https://github.com/yesodweb/wai/blob/master/warp-tls/README.md

KEY_EXPIRE = 3650
KEY_COUNTRY = AU
KEY_PROVINCE = NSW
KEY_CITY = Sydney
KEY_SIZE = 4096
KEY_SUBJECT = "/C=$(KEY_COUNTRY)/ST=$(KEY_PROVINCE)/L=$(KEY_CITY)/O=Dis/CN=localhost"
KEY_PASSWORD = "This is not so secret."

certificate.pem : certificate.csr
	openssl x509 -req -in certificate.csr -signkey key.pem -out $@

certificate.csr : key.pem
	openssl req -new -key key.pem \
		-days $(KEY_EXPIRE) \
		-subj $(KEY_SUBJECT) \
		-passout pass:$(KEY_PASSWORD) \
		-out $@

key.pem : create_certificate.mk
	openssl genrsa -out $@ $(KEY_SIZE)
