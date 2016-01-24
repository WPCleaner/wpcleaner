/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.dump;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;


/**
 * SAX handler for wiki dumps.
 */
public class DumpHandler extends DefaultHandler {

  /** Internal flag when parsing is in a page */
  public boolean isInPage;

  /** Handler for page information */
  public PageHandler pageHandler;

  public DumpHandler() {
    isInPage = false;
    pageHandler = new PageHandler();
  }

  /**
   * @param processor Page processor.
   */
  public void setPageProcessor(PageProcessor processor) {
    if (pageHandler != null) {
      pageHandler.setPageProcessor(processor);
    }
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
    if (!isInPage && qName.equalsIgnoreCase("page")) {
      isInPage = true;
    }
    if (isInPage) {
      if (pageHandler != null) {
        pageHandler.startElement(uri, localName, qName, attributes);
      }
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
      if (pageHandler != null) {
        pageHandler.endElement(uri, localName, qName);
      }
    }
    if (isInPage && qName.equalsIgnoreCase("page")) {
      isInPage = false;
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
      if (pageHandler != null) {
        pageHandler.characters(ch, start, length);
      }
    }
  }
}
