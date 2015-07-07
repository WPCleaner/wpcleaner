/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.xml;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiRevisionsResult;


/**
 * MediaWiki API XML revisions results.
 */
public class ApiXmlRevisionsResult extends ApiXmlPropertiesResult implements ApiRevisionsResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlRevisionsResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute last revision request.
   * 
   * @param properties Properties defining request.
   * @param pages Pages to be filled with last revision content.
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeLastRevision(
      Map<String, String> properties,
      Collection<Page> pages) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Manage redirects and missing pages
      updateRedirect(root, pages);

      // Retrieve pages
      XPath xpa = XPath.newInstance("/api/query/pages/page");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      while (iter.hasNext()) {
        Element pageNode = (Element) iter.next();
        String title = pageNode.getAttributeValue("title");
        Integer pageId = null;
        try {
          String pageIdValue = pageNode.getAttributeValue("pageid");
          if (pageIdValue != null) {
            pageId = Integer.valueOf(pageIdValue);
          }
        } catch (NumberFormatException e) {
          System.err.println("Incorrect page id");
        }
        String namespace = pageNode.getAttributeValue("ns");
        for (Page tmpPage : pages) {
          Iterator<Page> itPage = tmpPage.getRedirectIteratorWithPage();
          while (itPage.hasNext()) {
            Page page = itPage.next();
            boolean samePage = false;
            if ((pageId != null) && (page.getPageId() != null)) {
              samePage = pageId.equals(page.getPageId());
            } else {
              samePage = Page.areSameTitle(page.getTitle(), title);
            }
            if (samePage) {
              page.setNamespace(namespace);
              updatePageInformation(pageNode, page);
  
              // Retrieve revisions
              if (!Boolean.FALSE.equals(page.isExisting())) {
                XPath xpaRevisions = XPath.newInstance("revisions/rev");
                Element revNode = (Element) xpaRevisions.selectSingleNode(pageNode);
                if (revNode != null) {
                  page.setContents(revNode.getText());
                  page.setExisting(Boolean.TRUE);
                  page.setRevisionId(revNode.getAttributeValue("revid"));
                  page.setContentsTimestamp(revNode.getAttributeValue("timestamp"));
                }
              }
            }
          }
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/revisions",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading revisions", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
