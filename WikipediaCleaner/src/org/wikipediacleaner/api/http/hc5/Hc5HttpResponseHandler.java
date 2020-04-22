/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.http.hc5;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;

import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.Header;
import org.apache.hc.core5.http.HttpException;
import org.apache.hc.core5.http.HttpStatus;
import org.apache.hc.core5.http.io.HttpClientResponseHandler;
import org.apache.hc.core5.http.io.entity.EntityUtils;


/**
 * Response handler for HTTP requests.
 */
public class Hc5HttpResponseHandler
    implements HttpClientResponseHandler<Hc5HttpResponse> {

  /**
   * Default constructor.
   */
  public Hc5HttpResponseHandler() {
  }

  /**
   * @param response HTTP response.
   * @return HTTP response.
   * @throws HttpException Exception.
   * @throws IOException Exception.
   * @see org.apache.hc.core5.http.io.HttpClientResponseHandler#handleResponse(org.apache.hc.core5.http.ClassicHttpResponse)
   */
  @Override
  public Hc5HttpResponse handleResponse(ClassicHttpResponse response)
      throws HttpException, IOException {
    final int status = response.getCode();
    if ((status >= HttpStatus.SC_OK) && (status < HttpStatus.SC_REDIRECTION)) {
      byte[] data = EntityUtils.toByteArray(response.getEntity());
      InputStream is = new ByteArrayInputStream(data);
      Header hContentEncoding = response.getHeader("Content-Encoding");
      if ((hContentEncoding != null) && ("gzip".equals(hContentEncoding.getValue()))) {
        is = new GZIPInputStream(is);
      }
      return new Hc5HttpResponse(status, is);
    }
    return new Hc5HttpResponse(status);
  }
}
