/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.methods.GetMethod;
import org.wikipediacleaner.api.check.HtmlCharacters;


/**
 * A text provider for URL titles.
 */
public class TextProviderUrlTitle implements TextProvider {

  private final static Logger log = Logger.getLogger(TextProviderUrlTitle.class.getName());

  /**
   * URL. 
   */
  private final String url;

  /**
   * Maximum size of URL contents read to find title.
   */
  private final static int MAXIMUM_SIZE = 10000;

  /**
   * @param url URL.
   */
  public TextProviderUrlTitle(String url) {
    this.url = url;
  }

  /**
   * @return Possible texts.
   */
  @Override
  public Collection<String> getTexts() {
    log.fine("IN");
    Collection<String> result = new ArrayList<String>();
    if (url != null) {
      GetMethod method = null;
      InputStream is = null;
      try {
        log.fine("new HttpClient()");
        HttpClient httpClient = new HttpClient();
        method = new GetMethod(url);
        //System.out.println(url);
        int statusCode = httpClient.executeMethod(method);
        if (statusCode == HttpStatus.SC_OK) {
          log.fine("HttpStatus.SC_OK");
          is = method.getResponseBodyAsStream();
          byte[] tmpBytes = new byte[MAXIMUM_SIZE];
          int size = is.read(tmpBytes);
          Charset utf8 = Charset.forName("UTF8");
          String text = new String(tmpBytes, 0, size, utf8).replaceAll("\\s", " ");
          Pattern pCharset = Pattern.compile(
              "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=([^\"]+?)\"",
              Pattern.CASE_INSENSITIVE);
          Matcher m = pCharset.matcher(text);
          if (m.find() == true) {
            String charsetName = m.group(1).trim();
            try {
              Charset charset = Charset.forName(charsetName);
              if (!charset.equals(utf8)) {
                text = new String(tmpBytes, 0, size, charset).replaceAll("\\s", " ");
              }
            } catch (Exception e) {
              //
            }
          }
          Pattern[] pTitles = {
              Pattern.compile("<title>(.+?)</title>", Pattern.CASE_INSENSITIVE),
              Pattern.compile("<meta name=\"title\" content=\"([^\"]+?)\"", Pattern.CASE_INSENSITIVE),
              Pattern.compile("<meta name=\"description\" content=\"([^\"]+?)\"", Pattern.CASE_INSENSITIVE)
          };
          for (Pattern pTitle : pTitles) {
            m = pTitle.matcher(text);
            if (m.find() == true) {
              String title = m.group(1).trim();
              for (HtmlCharacters htmlChar : HtmlCharacters.values()) {
                if (!HtmlCharacters.SYMBOL_AMPERSAND.equals(htmlChar)) {
                  title = title.replaceAll("&#" + htmlChar.getNumber() + ";", "" + htmlChar.getValue());
                  if (htmlChar.getNumber() != htmlChar.getAlternativeNumber()) {
                    title = title.replaceAll("&#" + htmlChar.getAlternativeNumber() + ";", "" + htmlChar.getValue());
                  }
                  if (htmlChar.getName() != null) {
                    title = title.replaceAll("&" + htmlChar.getName() + ";", "" + htmlChar.getValue());
                  }
                }
              }
              result.add(title);
            }
          }
        }
      } catch (IOException ex) {
        log.fine("IOException");
        // Nothing to do
      } catch (Exception ex) {
        log.fine("Exception");
        // Nothing to do
      } finally {
        log.fine("finally");
        if (is != null) {
          try {
            is.close();
          } catch (IOException ex) {
            // Nothing to do 
          }
        }
        if (method != null) {
          method.releaseConnection();
        }
      }
    }
    log.fine("OUT");
    return result;
  }

}
