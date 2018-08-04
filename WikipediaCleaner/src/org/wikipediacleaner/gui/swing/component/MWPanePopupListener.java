/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.text.BadLocationException;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.ISBNRange;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementISSN;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementParameter;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.ISBNRange.ISBNInformation;
import org.wikipediacleaner.api.data.SearchEngine;
import org.wikipediacleaner.api.data.contents.ContentsComment;
import org.wikipediacleaner.api.data.contents.ContentsElement;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.menu.BasicMenuCreator;
import org.wikipediacleaner.i18n.GT;


/**
 * A popup menu listener for MediaWikiPane. 
 */
public abstract class MWPanePopupListener extends AbstractPopupListener {

  private final EnumWikipedia wikipedia;
  private BasicWindow window;

  public MWPanePopupListener(EnumWikipedia wikipedia, BasicWindow window) {
    this.wikipedia = wikipedia;
    this.window = window;
  }

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    return wikipedia;
  }

  /**
   * @return Window.
   */
  public BasicWindow getWindow() {
    return window;
  }

  /**
   * Show popup menu in response to a mouse event.
   * 
   * @param e Event.
   */
  @Override
  protected void showPopup(MouseEvent e) {

    // Retrieve information
    if (!(e.getComponent() instanceof MWPane)) {
      return;
    }
    MWPane textPane = (MWPane) e.getComponent();
    showPopup(textPane, textPane.viewToModel(e.getPoint()), e.getX(), e.getY());
  }

  /**
   * Show popup menu in response to a key event.
   * 
   * @param e Event.
   */
  @Override
  protected void showPopup(KeyEvent e) {

    // Retrieve information
    if (!(e.getComponent() instanceof MWPane)) {
      return;
    }
    MWPane textPane = (MWPane) e.getComponent();
    try {
      Rectangle rect = textPane.modelToView(textPane.getCaretPosition());
      showPopup(textPane, textPane.getSelectionStart(), rect.x, rect.y);
    } catch (BadLocationException e1) {
      //
    }
  }

  /**
   * Construct and show popup menu.
   * 
   * @param textPane Text pane.
   * @param position Position in text.
   * @param x Position.
   * @param y Position.
   */
  private void showPopup(MWPane textPane, int position, int x, int y) {

    // Basic checks
    if (textPane == null) {
      return;
    }

    // Create popup menu
    Page originalPage = textPane.getWikiPage();
    PageAnalysis pageAnalysis = originalPage.getAnalysis(textPane.getText(), true);
    JPopupMenu popup = createPopup(textPane, position, pageAnalysis);
    if (popup == null) {
      popup = createDefaultPopup(textPane, position, pageAnalysis);
    }

    // Display popup menu
    if (popup != null) {
      popup.show(textPane, x, y);
    }
  }

  /**
   * Construct popup menu.
   * 
   * @param textPane Text pane.
   * @param position Position in text.
   * @param pageAnalysis Page analysis.
   * @return Popup menu.
   */
  protected abstract JPopupMenu createPopup(
      MWPane textPane, int position,
      PageAnalysis pageAnalysis);

  /* ===================================================================== */
  /* Default implementation                                                */
  /* ===================================================================== */

  /**
   * Create a default popup menu.
   * 
   * @param textPane Text pane.
   * @param position Position in the text.
   * @param pageAnalysis Page analysis.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopup(
      MWPane textPane, int position,
      PageAnalysis pageAnalysis) {

    // Basic checks
    if (pageAnalysis == null) {
      return null;
    }

    // Find where the user has clicked
    ContentsElement element = pageAnalysis.isInElement(position);

    // Comment
    if (element instanceof ContentsComment) {
      return null;
    }

    // Menu for internal link
    if (element instanceof PageElementInternalLink) {
      PageElementInternalLink internalLink = (PageElementInternalLink) element;
      return createDefaultPopupInternalLink(textPane, position, pageAnalysis, internalLink);
    }

    // Menu for image
    if (element instanceof PageElementImage) {
      PageElementImage image = (PageElementImage) element;
      return createDefaultPopupImage(textPane, position, pageAnalysis, image);
    }

    // Menu for external link
    if (element instanceof PageElementExternalLink) {
      PageElementExternalLink externalLink = (PageElementExternalLink) element;
      return createDefaultPopupExternalLink(pageAnalysis, position, externalLink);
    }

    // Menu for template
    if (element instanceof PageElementTemplate) {
      PageElementTemplate template = (PageElementTemplate) element;
      return createDefaultPopupTemplate(pageAnalysis, position, template);
    }

    // Menu for category
    if (element instanceof PageElementCategory) {
      PageElementCategory category = (PageElementCategory) element;
      return createDefaultPopupCategory(textPane, position, pageAnalysis, category);
    }

    // Menu for interwiki
    if (element instanceof PageElementInterwikiLink) {
      PageElementInterwikiLink interwiki = (PageElementInterwikiLink) element;
      return createDefaultPopupInterwikiLink(textPane, position, pageAnalysis, interwiki);
    }

    // Menu for language
    if (element instanceof PageElementLanguageLink) {
      PageElementLanguageLink language = (PageElementLanguageLink) element;
      return createDefaultPopupLanguageLink(textPane, position, pageAnalysis, language);
    }

    // Menu for parameter
    if (element instanceof PageElementParameter) {
      PageElementParameter parameter = (PageElementParameter) element;
      return createDefaultPopupParameter(pageAnalysis, position, parameter);
    }

    // Menu for function
    if (element instanceof PageElementFunction) {
      PageElementFunction function = (PageElementFunction) element;
      return createDefaultPopupFunction(pageAnalysis, position, function);
    }

    // Menu for ISBN
    if (element instanceof PageElementISBN) {
      PageElementISBN isbn = (PageElementISBN) element;
      return createDefaultPopupISBN(pageAnalysis, position, isbn);
    }

    // Menu for ISSN
    if (element instanceof PageElementISSN) {
      PageElementISSN issn = (PageElementISSN) element;
      return createDefaultPopupISSN(pageAnalysis, position, issn);
    }

    // Default menu
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "Page: {0}",
        limitTextLength(pageAnalysis.getPage().getTitle(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);
    menu.addSeparator(popup);
    menu.addView(wikipedia, popup, pageAnalysis.getPage(), false);
    return popup;
  }

  /**
   * Create a default popup menu for an internal link.
   * 
   * @param textPane Text pane.
   * @param position Position in the text.
   * @param pageAnalysis Page analysis.
   * @param link Internal link.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupInternalLink(
      MWPane textPane, int position,
      PageAnalysis pageAnalysis,
      PageElementInternalLink link) {
    if (link == null) {
      return null;
    }

    // Initialization
    Page page = DataManager.getPage(wikipedia, link.getLink(), null, null, null);

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "Page: {0}",
        limitTextLength(page.getTitle(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);
    menu.addSeparator(popup);
    menu.addView(wikipedia, popup, page, false);
    menu.addAnalyze(wikipedia, popup, page);
    menu.addDisambiguation(wikipedia, popup, page);
    menu.addSeparator(popup);
    menu.addItemRemoveLink(
        popup, link.getDisplayedText(),
        textPane, link.getBeginIndex(), link.getEndIndex());

    return popup;
  }

  /**
   * Create a default popup menu for an interwiki link.
   * 
   * @param textPane Text pane.
   * @param position Position in the text.
   * @param pageAnalysis Page analysis.
   * @param link Interwiki link.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupInterwikiLink(
      MWPane textPane, int position,
      PageAnalysis pageAnalysis,
      PageElementInterwikiLink link) {
    if (link == null) {
      return null;
    }

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "Interwiki: {0}",
        limitTextLength(link.getInterwikiText() + ":" + link.getLink(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);

    return popup;
  }

  /**
   * Create a default popup menu for a language link.
   * 
   * @param textPane Text pane.
   * @param position Position in the text.
   * @param pageAnalysis Page analysis.
   * @param link Language link.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupLanguageLink(
      MWPane textPane, int position,
      PageAnalysis pageAnalysis,
      PageElementLanguageLink link) {
    if (link == null) {
      return null;
    }

    // Initialization
    EnumWikipedia otherWikipedia = EnumWikipedia.getWikipedia(link.getLanguage());
    Page otherPage = null;
    if (otherWikipedia != null) {
      otherPage = DataManager.getPage(otherWikipedia, link.getLink(), null, null, null);
    }

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(
        link.getLanguage() + ":" +
        limitTextLength(link.getLink(), 50));
    menu.addCurrentChapter(popup, position, pageAnalysis);
    if (otherWikipedia != null) {
      menu.addSeparator(popup);
      menu.addView(otherWikipedia, popup, otherPage, false);
    }

    return popup;
  }

  /**
   * Create a default popup menu for an image.
   * 
   * @param textPane Text pane.
   * @param position Position in the text.
   * @param pageAnalysis Page analysis.
   * @param image Image.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupImage(
      MWPane textPane, int position,
      PageAnalysis pageAnalysis,
      PageElementImage image) {
    if (image == null) {
      return null;
    }

    // Initialization
    String fullName = wikipedia.getWikiConfiguration().getPageTitle(
        Namespace.IMAGE,
        image.getImage());
    Page page = DataManager.getPage(wikipedia, fullName, null, null, null);

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "Image: {0}",
        limitTextLength(image.getImage(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);
    menu.addSeparator(popup);
    menu.addView(wikipedia, popup, page, false);
    menu.addAnalyze(wikipedia, popup, page);

    return popup;
  }

  /**
   * Create a default popup menu for a category.
   * 
   * @param textPane Text pane.
   * @param position Position in the text.
   * @param pageAnalysis Page analysis.
   * @param category Category.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupCategory(
      MWPane textPane, int position,
      PageAnalysis pageAnalysis,
      PageElementCategory category) {
    if (category == null) {
      return null;
    }

    // Initialization
    String fullName = wikipedia.getWikiConfiguration().getPageTitle(
        Namespace.CATEGORY,
        category.getName());
    Page page = DataManager.getPage(wikipedia, fullName, null, null, null);

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "Category: {0}",
        limitTextLength(category.getName(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);
    menu.addSeparator(popup);
    menu.addView(wikipedia, popup, page, false);
    menu.addAnalyze(wikipedia, popup, page);

    return popup;
  }

  /**
   * Create a default popup menu for an external link.
   * 
   * @param pageAnalysis Page analysis.
   * @param position Position in the text.
   * @param link External link.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupExternalLink(
      PageAnalysis pageAnalysis, int position,
      PageElementExternalLink link) {
    if (link == null) {
      return null;
    }

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "External link: {0}",
        limitTextLength(link.getLink(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);
    menu.addSeparator(popup);
    menu.addItemView(null, popup, link.getLink());

    return popup;
  }

  /**
   * Create a default popup menu for a template.
   * 
   * @param pageAnalysis Page analysis.
   * @param position Position in the text.
   * @param template Template.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupTemplate(
      PageAnalysis pageAnalysis, int position,
      PageElementTemplate template) {
    if (template == null) {
      return null;
    }

    // Initialization
    String fullName = wikipedia.getWikiConfiguration().getPageTitle(
        Namespace.TEMPLATE,
        template.getTemplateName());
    Page page = DataManager.getPage(wikipedia, fullName, null, null, null);

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "Template: {0}",
        limitTextLength(template.getTemplateName(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);

    // Search engines
    Map<String, List<SearchEngine>> searchEngines = SearchEngine.getSearchEngines(
        wikipedia, template, null);
    if ((searchEngines != null) && !searchEngines.isEmpty()) {
      menu.addSeparator(popup);
      List<String> parameterNames = new ArrayList<>(searchEngines.keySet());
      Collections.sort(parameterNames);
      for (String parameterName : parameterNames) {
        JMenu submenu = new JMenu(GT._T("Search using {0}", parameterName));
        for (SearchEngine searchEngine : searchEngines.get(parameterName)) {
          menu.addItemView(null, submenu, searchEngine.getUrl(), searchEngine.getName());
        }
        menu.addSubmenu(popup, submenu, 0, 0);
      }
    }

    // General items
    menu.addSeparator(popup);
    menu.addCheckTemplate(wikipedia, window.getParentComponent(), popup, template);
    menu.addView(wikipedia, popup, page, false);
    menu.addAnalyze(wikipedia, popup, page);

    return popup;
  }

  /**
   * Create a default popup menu for a parameter.
   * 
   * @param pageAnalysis Page analysis.
   * @param position Position in the text.
   * @param parameter Parameter.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupParameter(
      PageAnalysis pageAnalysis, int position,
      PageElementParameter parameter) {
    if (parameter == null) {
      return null;
    }

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "Parameter: {0}",
        limitTextLength(parameter.getParameterName(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);

    return popup;
  }

  /**
   * Create a default popup menu for a function.
   * 
   * @param pageAnalysis Page analysis.
   * @param position Position in the text.
   * @param function Function.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupFunction(
      PageAnalysis pageAnalysis, int position,
      PageElementFunction function) {
    if (function == null) {
      return null;
    }

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "Function: {0}",
        limitTextLength(function.getFunctionName(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);

    return popup;
  }

  /**
   * Create a default popup menu for an ISBN.
   * 
   * @param pageAnalysis Page analysis.
   * @param position Position in the text.
   * @param isbn ISBN.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupISBN(
      PageAnalysis pageAnalysis, int position,
      PageElementISBN isbn) {
    if (isbn == null) {
      return null;
    }

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "ISBN: {0}",
        limitTextLength(isbn.getISBN(), 50)));
    ISBNInformation infos = ISBNRange.getInformation(isbn.getISBN());
    if ((infos != null) && (infos.getTexts() != null)) {
      for (String info : infos.getTexts()) {
        menu.addDisabledText(popup, info);
      }
    }
    menu.addCurrentChapter(popup, position, pageAnalysis);

    return popup;
  }

  /**
   * Create a default popup menu for an ISSN.
   * 
   * @param pageAnalysis Page analysis.
   * @param position Position in the text.
   * @param issn ISSN.
   * @return Popup menu.
   */
  protected JPopupMenu createDefaultPopupISSN(
      PageAnalysis pageAnalysis, int position,
      PageElementISSN issn) {
    if (issn == null) {
      return null;
    }

    // Menu creation
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._T(
        "ISSN: {0}",
        limitTextLength(issn.getISSN(), 50)));
    menu.addCurrentChapter(popup, position, pageAnalysis);

    return popup;
  }

  /**
   * Limit text length to be used in menus.
   * 
   * @param text Original text.
   * @param maxLength Maximum length.
   * @return Text with length restricted to maximum length.
   */
  protected String limitTextLength(String text, int maxLength) {
    if (text == null) {
      return null;
    }
    if (maxLength < 10) {
      maxLength = 10;
    }
    if (text.length() <= maxLength) {
      return text;
    }
    int middle = (maxLength - 3) / 2;
    text = text.substring(0, maxLength - middle) +
           "..." +
           text.substring(text.length() - middle);
    return text;
  }
}
