/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.dump;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageRedirect;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;


/**
 * SAX handler for page elements.
 */
public class PageHandler extends DefaultHandler {

  /** Logger */
  private final Logger log = LoggerFactory.getLogger(PageHandler.class);

  /** Number of pages processed */
  private long pageCount;

  /** True when parsing a page */
  private boolean isInPage;

  /** True when parsing a title */
  private boolean isInTitle;

  /** Page title */
  private StringBuilder title;

  /** True when parsing name space */
  private boolean isInNamespace;

  /** Name space */
  private StringBuilder namespace;

  /** True when parsing a page id */
  private boolean isInPageId;

  /** Page id */
  private StringBuilder pageId;

  /** Redirect */
  private StringBuilder redirect;

  /** True when parsing a revision */
  private boolean isInRevision;

  /** True when parsing a revision id */
  private boolean isInRevisionId;

  /** Revision id */
  private StringBuilder revisionId;

  /** True when parsing a revision text */
  private boolean isInRevisionText;

  /** Revision text */
  private StringBuilder revisionText;

  /** Page processor */
  private PageProcessor processor;

  /**
   * Constructor.
   */
  public PageHandler() {
    pageCount = 0;
    isInPage = false;
    title = new StringBuilder();
    namespace = new StringBuilder();
    pageId = new StringBuilder();
    redirect = new StringBuilder();
    revisionId = new StringBuilder();
    revisionText = new StringBuilder();
    cleanPageInformation();
  }

  /**
   * @param processor Page processor.
   */
  public void setPageProcessor(PageProcessor processor) {
    this.processor = processor;
  }

  /**
   * Receive notification of the start of an element.
   *
   * @param uri The Namespace URI, or the empty string if the
   *        element has no Namespace URI or if Namespace
   *        processing is not being performed.
   * @param localName The local name (without prefix), or the
   *        empty string if Namespace processing is not being
   *        performed.
   * @param qName The qualified name (with prefix), or the
   *        empty string if qualified names are not available.
   * @param attributes The attributes attached to the element.  If
   *        there are no attributes, it shall be an empty
   *        Attributes object.
   * @exception org.xml.sax.SAXException Any SAX exception, possibly
   *            wrapping another exception.
   * @see org.xml.sax.ContentHandler#startElement
   */
  @Override
  public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
    if (isInPage) {
      if (isInRevision) {
        if (qName.equals("id")) {
          isInRevisionId = true;
          revisionId.setLength(0);
        } else if (qName.equals("text")) {
          isInRevisionText = true;
          revisionText.setLength(0);
        }
      } else if (qName.equalsIgnoreCase("title")) {
        isInTitle = true;
        title.setLength(0);
      } else if (qName.equalsIgnoreCase("ns")) {
        isInNamespace = true;
        namespace.setLength(0);
      } else if (qName.equalsIgnoreCase("id")) {
        isInPageId = true;
        pageId.setLength(0);
      } else if (qName.equalsIgnoreCase("redirect")) {
        redirect.setLength(0);
        String tmp = attributes.getValue("title");
        if (tmp != null) {
          redirect.append(tmp);
        }
      } else if (qName.equalsIgnoreCase("revision")) {
        isInRevision = true;
        isInRevisionId = false;
        revisionId.setLength(0);
        isInRevisionText = false;
        revisionText.setLength(0);
      }
    } else if (qName.equalsIgnoreCase("page")) {
      isInPage = true;
      cleanPageInformation();
    }
  }

  /**
   * Receive notification of the end of an element.
   *
   * @param uri The Namespace URI, or the empty string if the
   *        element has no Namespace URI or if Namespace
   *        processing is not being performed.
   * @param localName The local name (without prefix), or the
   *        empty string if Namespace processing is not being
   *        performed.
   * @param qName The qualified name (with prefix), or the
   *        empty string if qualified names are not available.
   * @exception org.xml.sax.SAXException Any SAX exception, possibly
   *            wrapping another exception.
   * @see org.xml.sax.ContentHandler#endElement
   */
  @Override
  public void endElement(String uri, String localName, String qName) throws SAXException {
    if (!isInPage) {
      return;
    }

    if (isInRevision) {
      if (qName.equalsIgnoreCase("revision")) {
        isInRevision = false;
        isInRevisionId = false;
      } else if (qName.equalsIgnoreCase("id")) {
        isInRevisionId = false;
      } else if (qName.equalsIgnoreCase("text")) {
        isInRevisionText = false;
      }
    } else if (qName.equalsIgnoreCase("page")) {
      if (processor != null) {
        increasePageCount();
        try {
          Integer namespaceId = Integer.valueOf(namespace.toString());
          Page currentPage = DataManager.createSimplePage(
              processor.getWiki(), title.toString(),
              Integer.valueOf(pageId.toString(), 10), revisionId.toString(),
              namespaceId);
          currentPage.setNamespace(namespaceId);
          currentPage.setContents(revisionText.toString());
          if (redirect.length() > 0) {
            PageRedirect redirects = currentPage.getRedirects();
            redirects.isRedirect(true);
            redirects.add(DataManager.createSimplePage(processor.getWiki(), redirect.toString(), null, null, null), null);
          }
          processor.processPage(currentPage);
        } catch (NumberFormatException e) {
          log.error("Problem in endElement: " + e.getMessage());
        }
      }
      isInPage = false;
      cleanPageInformation();
    } else if (qName.equalsIgnoreCase("title")) {
      isInTitle = false;
    } else if (qName.equalsIgnoreCase("ns")) {
      isInNamespace = false;
      if (processor != null) {
        try {
          Integer namespaceNum = Integer.valueOf(namespace.toString());
          if (!processor.isForNamespace(namespaceNum)) {
            increasePageCount();
            isInPage = false;
            cleanPageInformation();
          }
        } catch (NumberFormatException e) {
          log.error("Incorrect namespace {} for page {}", namespace, title);
        }
      }
    } else if (qName.equalsIgnoreCase("id")) {
      isInPageId = false;
    }
  }

  /**
   * Increase page count.
   */
  private void increasePageCount() {
    pageCount++;
    if (pageCount % 100000 == 0) {
      log.info("Dump parser has gone through {} pages", pageCount);
    }
  }

  /**
   * Receive notification of character data inside an element.
   *
   * @param ch The characters.
   * @param start The start position in the character array.
   * @param length The number of characters to use from the
   *               character array.
   * @exception org.xml.sax.SAXException Any SAX exception, possibly
   *            wrapping another exception.
   * @see org.xml.sax.ContentHandler#characters
   */
  @Override
  public void characters(char ch[], int start, int length) throws SAXException {
    if (isInPage) {
      if (isInRevision) {
        if (isInRevisionId) {
          revisionId.append(ch, start, length);
        } else if (isInRevisionText) {
          revisionText.append(ch, start, length);
        }
      } else if (isInTitle) {
        title.append(ch, start, length);
      } else if (isInNamespace) {
        namespace.append(ch, start, length);
      } else if (isInPageId) {
        pageId.append(ch, start, length);
      }
    }
  }

  /**
   * Clean current page information.
   */
  private void cleanPageInformation() {
    isInTitle = false;
    title.setLength(0);
    isInNamespace = false;
    namespace.setLength(0);
    isInPageId = false;
    pageId.setLength(0);
    redirect.setLength(0);
    isInRevision = false;
    isInRevisionId = false;
    revisionId.setLength(0);
    isInRevisionText = false;
    revisionText.setLength(0);
  }
}
