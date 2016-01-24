/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.dump;

import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;


/**
 * SAX handler for page elements.
 */
public class PageHandler extends DefaultHandler {

  /** True when parsing a page */
  private boolean isInPage;

  /** True when parsing a title */
  private boolean isInTitle;

  /** Page title */
  private String title;

  /** True when parsing namespace */
  private boolean isInNamespace;

  /** Namespace */
  private Integer namespace;

  /** True when parsing a page id */
  private boolean isInPageId;

  /** Page id */
  private Integer pageId;

  /** True when parsing a revision */
  private boolean isInRevision;

  /** True when parsing a revision id */
  private boolean isInRevisionId;

  /** Revision id */
  private Integer revisionId;

  /** True when parsing a revision text */
  private boolean isInRevisionText;

  /** Revision text */
  private String revisionText;

  /** Page processor */
  private PageProcessor processor;

  public PageHandler() {
    isInPage = false;
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
   * <p>By default, do nothing.  Application writers may override this
   * method in a subclass to take specific actions at the start of
   * each element (such as allocating a new tree node or writing
   * output to a file).</p>
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
          revisionId = null;
        } else if (qName.equals("text")) {
          isInRevisionText = true;
          revisionText = null;
        }
      } else if (qName.equalsIgnoreCase("title")) {
        isInTitle = true;
        title = null;
      } else if (qName.equalsIgnoreCase("ns")) {
        isInNamespace = true;
        namespace = null;
      } else if (qName.equalsIgnoreCase("id")) {
        isInPageId = true;
        pageId = null;
      } else if (qName.equalsIgnoreCase("revision")) {
        isInRevision = true;
        isInRevisionId = false;
        revisionId = null;
        isInRevisionText = false;
        revisionText = null;
      }
    } else if (qName.equalsIgnoreCase("page")) {
      isInPage = true;
      cleanPageInformation();
    }
  }

  /**
   * Receive notification of the end of an element.
   *
   * <p>By default, do nothing.  Application writers may override this
   * method in a subclass to take specific actions at the end of
   * each element (such as finalising a tree node or writing
   * output to a file).</p>
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
    if (isInPage) {
      if (qName.equalsIgnoreCase("page")) {
        if (processor != null) {
          Page page = DataManager.getPage(
              processor.getWiki(), title, pageId, revisionId.toString(), null);
          page.setNamespace(namespace);
          page.setContents(revisionText);
          processor.processPage(page);
        }
        isInPage = false;
        cleanPageInformation();
      } else if (isInRevision) {
        if (qName.equalsIgnoreCase("revision")) {
          isInRevision = false;
          isInRevisionId = false;
        } else if (qName.equalsIgnoreCase("id")) {
          isInRevisionId = false;
        } else if (qName.equalsIgnoreCase("text")) {
          isInRevisionText = false;
        }
      } else if (qName.equalsIgnoreCase("title")) {
        isInTitle = false;
      } else if (qName.equalsIgnoreCase("ns")) {
        isInNamespace = false;
      } else if (qName.equalsIgnoreCase("id")) {
        isInPageId = false;
      }
    }
  }

  /**
   * Receive notification of character data inside an element.
   *
   * <p>By default, do nothing.  Application writers may override this
   * method to take specific actions for each chunk of character data
   * (such as adding the data to a node or buffer, or printing it to
   * a file).</p>
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
          revisionId = Integer.valueOf(new String(ch, start, length));
        } else if (isInRevisionText) {
          if (revisionText == null) {
            revisionText = "";
          }
          revisionText += new String(ch, start, length);
        }
      } else if (isInTitle) {
        title = new String(ch, start, length);
      } else if (isInNamespace) {
        namespace = Integer.valueOf(new String(ch, start, length));
      } else if (isInPageId) {
        pageId = Integer.valueOf(new String(ch, start, length));
      }
    }
  }

  /**
   * Clean current page information.
   */
  private void cleanPageInformation() {
    isInTitle = false;
    title = null;
    isInNamespace = false;
    namespace = null;
    isInPageId = false;
    pageId = null;
    isInRevision = false;
    isInRevisionId = false;
    revisionId = null;
    isInRevisionText = false;
    revisionText = null;
  }
}
