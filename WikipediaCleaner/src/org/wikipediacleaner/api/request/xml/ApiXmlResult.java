/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.xml;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpStatus;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.JDOMParseException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.BasicApiResult;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * MediaWiki API XML results.
 */
public abstract class ApiXmlResult extends BasicApiResult {

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * Flag for tracing XML.
   */
  private static boolean DEBUG_XML = false;

  /**
   * Update configuration.
   */
  public static void updateConfiguration() {
    Configuration config = Configuration.getConfiguration();
    DEBUG_XML = config.getBoolean(
        null, ConfigurationValueBoolean.DEBUG_API);
  }

  // ==========================================================================
  // XML Results
  // ==========================================================================

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * @return Format of the XML result.
   */
  @Override
  public String getFormat() {
    return ApiRequest.FORMAT_XML;
  }

  /**
   * Send a request to MediaWiki API.
   * 
   * @param properties Properties defining the request.
   * @param maxTry Maximum number of tries.
   * @return Answer of MediaWiki API.
   * @throws JDOMParseException
   * @throws APIException
   */
  protected Element getRoot(
      Map<String, String> properties,
      int maxTry)
          throws JDOMParseException, APIException {
    int attempt = 0;
    for (;;) {
      Element root = null;
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
        // for (Header header : method.getRequestHeaders()) {
        //   System.out.println("Request header:" + header);
        // }
        // for (Header header : method.getResponseHeaders()) {
        //   System.out.println("Response header:" + header);
        // }

        // Read the response
        if (statusCode == HttpStatus.SC_OK){
          SAXBuilder sxb = new SAXBuilder();
          Document document = sxb.build(stream);
          traceDocument(document);
          root = document.getRootElement();
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
      } catch (JDOMException e) {
        String message = "JDOMException: " + e.getMessage();
        log.error(message);
        if (attempt > maxTry) {
          log.warn("Error. Maximum attempts count reached.");
          throw new APIException("Error parsing XML result", e);
        }
        try {
          Thread.sleep(30000);
        } catch (InterruptedException e2) {
          // Nothing
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
   * @throws APIException
   */
  protected void checkForError(Element root) throws APIException {
    if (root == null) {
      return;
    }
    
    // Check for errors
    try {
      XPath xpa = XPath.newInstance("/api/error");
      List listErrors = xpa.selectNodes(root);
      if (listErrors != null) {
        Iterator iterErrors = listErrors.iterator();
        XPath xpaCode = XPath.newInstance("./@code");
        XPath xpaInfo = XPath.newInstance("./@info");
        while (iterErrors.hasNext()) {
          Element currentNode = (Element) iterErrors.next();
          String text = "Error reported: " + xpaCode.valueOf(currentNode) + " - " + xpaInfo.valueOf(currentNode);
          log.warn(text);
          throw new APIException(text, xpaCode.valueOf(currentNode));
        }
      }
    } catch (JDOMException e) {
      log.error("JDOMException: " + e.getMessage());
    }
    
    // Check for warnings
    try {
      XPath xpa = XPath.newInstance("/api/warnings/*");
      List listWarnings = xpa.selectNodes(root);
      if (listWarnings != null) {
        Iterator iterWarnings = listWarnings.iterator();
        while (iterWarnings.hasNext()) {
          Element currentNode = (Element) iterWarnings.next();
          log.warn("Warning reported: " + currentNode.getName() + " - " + currentNode.getValue());
        }
      }
    } catch( JDOMException e) {
      log.error("JDOMException: " + e.getMessage());
    }
  }

  /**
   * Manage query-continue in request.
   * 
   * @param root Root of the DOM tree.
   * @param queryContinue XPath query to the query-continue node.
   * @param properties Properties defining request.
   * @return True if request should be continued.
   */
  protected boolean shouldContinue(
      Element root, String queryContinue,
      Map<String, String> properties) {
    if ((root == null) || (queryContinue == null)) {
      return false;
    }
    boolean result = false;
    try {
      XPath xpa = XPath.newInstance(queryContinue);
      List results = xpa.selectNodes(root);
      if ((results == null) || (results.isEmpty())) {
        xpa = XPath.newInstance("/api/continue");
        results = xpa.selectNodes(root);
      }
      if (results != null) {
        for (Object currentNode : results) {
          List attributes = ((Element) currentNode).getAttributes();
          if (attributes != null) {
            for (Object currentAttribute : attributes) {
              Attribute attribute = (Attribute) currentAttribute;
              properties.put(attribute.getName(), attribute.getValue());
              result = true;
            }
          }
        }
      }
    } catch (JDOMException e) {
      log.error("Error analyzing query-continue", e);
      return false;
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
   * Utility method to create a XPath.
   * 
   * @param element Element.
   * @param attribute Attribute.
   * @param value Attribute value.
   * @return XPath
   * @throws JDOMException
   */
  protected static XPath createXPath(
      String element,
      String attribute,
      String value)
      throws JDOMException {
    if ((value != null) && (value.indexOf("\"") != -1)) {
      return XPath.newInstance(element + "[@" + attribute + "=\"" + xmlOutputter.escapeAttributeEntities(value) + "\"]");
    }
    return XPath.newInstance(element + "[@" + attribute + "=\"" + value + "\"]");
  }

  /**
   * Formatter for XML output.
   */
  private static XMLOutputter xmlOutputter = new XMLOutputter(Format.getPrettyFormat());

  /**
   * Trace a document contents.
   * 
   * @param doc Document.
   */
  private void traceDocument(Document doc) {
    if (DEBUG_XML) {
      if (xmlOutputter == null) {
        xmlOutputter = new XMLOutputter(Format.getPrettyFormat());
      }
      try {
        System.out.println("********** START OF DOCUMENT **********");
        xmlOutputter.output(doc, System.out);
        System.out.println("**********  END OF DOCUMENT  **********");
      } catch (IOException e) {
        // Nothing to do
      }
    }
  }
}
