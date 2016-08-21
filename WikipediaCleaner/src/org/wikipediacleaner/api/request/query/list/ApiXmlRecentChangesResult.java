/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.filter.Filters;
import org.jdom2.xpath.XPathExpression;
import org.jdom2.xpath.XPathFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.RecentChange;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML recent changes results.
 */
public class ApiXmlRecentChangesResult extends ApiXmlResult implements ApiRecentChangesResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlRecentChangesResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute recent changes request.
   * 
   * @param properties Properties defining request.
   * @param recentChanges The list of recent changes to be filled.
   * @return The timestamp to use as a starting point for the next call.
   * @throws APIException
   */
  @Override
  public String executeRecentChanges(
      Map<String, String> properties,
      List<RecentChange> recentChanges) throws APIException {
    String nextStart = null;
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Get recent changes list
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/recentchanges/rc", Filters.element());
      List<Element> results = xpa.evaluate(root);
      Iterator<Element> iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        boolean isAnonymous = currentNode.getAttribute("anon") != null;
        boolean isBot = currentNode.getAttribute("bot") != null;
        boolean isMinor = currentNode.getAttribute("minor") != null;
        boolean isNew = currentNode.getAttribute("new") != null;
        boolean isRedirect = currentNode.getAttribute("redirect") != null;
        String comment = currentNode.getAttributeValue("comment");
        String ns = currentNode.getAttributeValue("ns");
        String pageId = currentNode.getAttributeValue("pageid");
        String rcId = currentNode.getAttributeValue("rcid");
        String revId = currentNode.getAttributeValue("revid");
        String timestamp = currentNode.getAttributeValue("timestamp");
        if (nextStart == null) {
          nextStart = timestamp;
        }
        String title = currentNode.getAttributeValue("title");
        String type = currentNode.getAttributeValue("type");
        String user = currentNode.getAttributeValue("user");
        String logType = currentNode.getAttributeValue("logtype");
        String logAction = currentNode.getAttributeValue("logaction");
        try {
          RecentChange rc = new RecentChange(
              Integer.valueOf(rcId), Integer.valueOf(ns),
              title, Integer.valueOf(pageId),
              Integer.valueOf(revId));
          rc.setAnonymous(isAnonymous);
          rc.setBot(isBot);
          rc.setComment(comment);
          rc.setLogAction(logAction);
          rc.setLogType(logType);
          rc.setMinor(isMinor);
          rc.setNew(isNew);
          rc.setRedirect(isRedirect);
          rc.setTimestamp(timestamp);
          rc.setType(type);
          rc.setUser(user);
          recentChanges.add(0, rc);
        } catch (NumberFormatException e) {
          log.error("Error loading recent changes", e);
        }
      }
    } catch (JDOMException e) {
      log.error("Error loading recent changes", e);
      throw new APIException("Error parsing XML", e);
    }

    return nextStart;
  }
}
