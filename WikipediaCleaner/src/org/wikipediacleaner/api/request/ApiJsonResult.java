/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpStatus;
import org.jdom2.Attribute;
import org.jdom2.Element;
import org.jdom2.input.JDOMParseException;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;


/**
 * MediaWiki API JSON results.
 */
public abstract class ApiJsonResult extends BasicApiResult {

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * Flag for tracing JSON.
   */
  private static boolean DEBUG_JSON = false;

  /**
   * JSON factory.
   */
  protected final static JsonFactory factory = new JsonFactory();

  /**
   * Update configuration.
   */
  public static void updateConfiguration() {
    Configuration config = Configuration.getConfiguration();
    DEBUG_JSON = config.getBoolean(
        null, ConfigurationValueBoolean.DEBUG_API);
  }

  // ==========================================================================
  // XML Results
  // ==========================================================================

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiJsonResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * @return Format of the JSON result.
   */
  @Override
  public String getFormat() {
    return ApiRequest.FORMAT_JSON;
  }

  /**
   * Send a request to MediaWiki API.
   * 
   * @param properties Properties defining the request.
   * @param maxTry Maximum number of tries.
   * @return Answer of MediaWiki API.
   * @throws APIException Exception thrown by the API.
   */
  protected JsonNode getRoot(
      Map<String, String> properties,
      int maxTry)
          throws APIException {
    int attempt = 0;
    for (;;) {
      JsonNode root = null;
      HttpMethod method = null;
      InputStream stream = null;
      try {
        // Executing HTTP method
        attempt++;
        method = createHttpMethod(properties);
        int statusCode = getHttpClient().executeMethod(method);

        // Accessing response
        stream = method.getResponseBodyAsStream();
        stream = new BufferedInputStream(stream);
        Header contentEncoding = method.getResponseHeader("Content-Encoding");
        if (contentEncoding != null) {
          if (contentEncoding.getValue().equals("gzip")) {
            stream = new GZIPInputStream(stream);
          }
        }

        // Read the response
        if (statusCode == HttpStatus.SC_OK){
          ObjectMapper mapper = new ObjectMapper(factory);
          root = mapper.readValue(stream, JsonNode.class);
          traceDocument(root);
          checkForError(root);
        } else {
          try {
            while (stream.read() >= 0) {
              //
            }
          } catch (IOException e) {
            //
          }
        }

        // Act depending on the status
        if (statusCode != HttpStatus.SC_OK) {
          String message = "URL access returned " + HttpStatus.getStatusText(statusCode);
          log.error(message);
          if (attempt > maxTry) {
            log.warn("Error. Maximum attempts count reached.");
            throw new APIException(message);
          }
          try {
            Thread.sleep(30000);
          } catch (InterruptedException e) {
            // Nothing
          }
        } else {
          return root;
        }
      } catch (IOException e) {
        String message = "IOException: " + e.getMessage();
        log.error(message);
        if (attempt > maxTry) {
          log.warn("Error. Maximum attempts count reached.");
          throw new APIException("Error accessing MediaWiki", e);
        }
        try {
          Thread.sleep(30000);
        } catch (InterruptedException e2) {
          // Nothing
        }
      } catch (APIException e) {
        if (!e.shouldRetry() || (attempt > e.getMaxRetry())) {
          throw e;
        }
        e.waitForRetry();
      } finally {
        if (stream != null) {
          try {
            stream.close();
          } catch (IOException e) {
            log.warn("Error closing stream");
          }
        }
        if (method != null) {
          method.releaseConnection();
        }
      }
      log.warn("Error. Trying again");
    }
  }

  /**
   * Check for errors reported by the API.
   * 
   * @param root Document root.
   * @throws APIException Exception thrown by the API.
   */
  protected void checkForError(JsonNode root) throws APIException {
    if (root == null) {
      return;
    }
    
    // Check for errors
    JsonNode error = root.path("error");
    if ((error != null) && !error.isMissingNode()) {
      String code = error.path("code").asText("?");
      String info = error.path("info").asText("?");
      String text = "Error reported: " + code + " - " + info;
      log.warn(text);
      throw new APIException(text, code);
    }

    // Check for warnings
    JsonNode warnings = root.path("warnings");
    if ((warnings != null) && !warnings.isMissingNode()) {
      log.warn("Warning reported: ");
      String query = warnings.path("query").asText();
      if (query != null) {
        log.warn(query);
      }
      String info = warnings.path("info").asText();
      if (info != null) {
        log.warn(info);
      }
    }
  }

  /**
   * Manage query-continue in request.
   * 
   * @param root Root of the JSON tree.
   * @param queryContinue XPath query to the query-continue node.
   * @param properties Properties defining request.
   * @return True if request should be continued.
   */
  protected boolean shouldContinue(
      JsonNode root, String queryContinue,
      Map<String, String> properties) {
    if ((root == null) || (queryContinue == null)) {
      return false;
    }
    boolean result = false;
    JsonNode continueNode = root.path("continue");
    if ((continueNode != null) && !continueNode.isMissingNode()) {
      Iterator<Entry<String, JsonNode>> continueIterator = continueNode.fields();
      while (continueIterator.hasNext()) {
        Entry<String, JsonNode> continueElement = continueIterator.next();
        String name = continueElement.getKey();
        String value = continueElement.getValue().asText();
        if ((name != null) && (value != null)) {
          properties.put(name, value);
          if (!"".equals(value)) {
            result = true;
          }
        }
      }
    }
    return result;
  }

  /**
   * Get a page corresponding to a page node.
   * 
   * @param wiki Wiki.
   * @param pageNode Page node.
   * @param knownPages Already known pages.
   * @param useDisambig True if disambiguation property should be used.
   * @return Page.
   */
  protected static Page getPage(
      EnumWikipedia wiki,
      Element pageNode, List<Page> knownPages,
      boolean useDisambig) {
    if (pageNode == null) {
      return null;
    }
    String title = pageNode.getAttributeValue("title");
    Attribute pageIdAttr = pageNode.getAttribute("pageid");
    Integer pageId = null;
    if (pageIdAttr != null) {
      try {
        String tmp = pageIdAttr.getValue();
        pageId = Integer.valueOf(tmp);
      } catch (NumberFormatException e) {
        //
      }
    }
    String revisionId = pageNode.getAttributeValue("lastrevid");
    Page page = DataManager.getPage(wiki, title, pageId, revisionId, knownPages);
    page.setNamespace(pageNode.getAttributeValue("ns"));
    if (pageNode.getAttribute("missing") != null) {
      page.setExisting(Boolean.FALSE);
    } else if (pageId != null) {
      page.setExisting(Boolean.TRUE);
    }
    if (pageNode.getAttribute("redirect") != null) {
      page.isRedirect(true);
    }
    if (useDisambig) {
      Element pageProps = pageNode.getChild("pageprops");
      boolean dabPage = (pageProps != null) && (pageProps.getAttribute("disambiguation") != null);
      page.setDisambiguationPage(Boolean.valueOf(dabPage));
    }
    return page;
  }

  /**
   * Trace a document contents.
   * 
   * @param parser JSON parser.
   */
  private void traceDocument(JsonNode root) {
    if (DEBUG_JSON) {
      System.out.println("********** START OF DOCUMENT **********");
      if (root != null) {
        System.out.println(root.toString());
      }
      System.out.println("**********  END OF DOCUMENT  **********");
    }
  }
}
