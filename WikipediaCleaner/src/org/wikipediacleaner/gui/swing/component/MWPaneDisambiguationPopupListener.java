/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import javax.swing.AbstractButton;
import javax.swing.JPopupMenu;
import javax.swing.text.AttributeSet;
import javax.swing.text.Element;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.TemplateMatcher;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.menu.MWPaneDisambiguationMenuCreator;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * A popup menu listener for MediaWikiPane for disambiguation.
 */
public class MWPaneDisambiguationPopupListener extends MWPanePopupListener {

  public MWPaneDisambiguationPopupListener(
      EnumWikipedia wikipedia, BasicWindow window) {
    super(wikipedia, window);
  }

  /**
   * Construct popup menu.
   * 
   * @param textPane Text pane.
   * @param position Position in text.
   * @param pageAnalysis Page analysis.
   */
  @Override
  protected JPopupMenu createPopup(
      MWPane textPane, int position,
      PageAnalysis pageAnalysis) {
    if ((textPane == null) || (pageAnalysis == null)) {
      return null;
    }
    Element element = textPane.getStyledDocument().getCharacterElement(position);
    if (element == null) {
      return null;
    }
    int startOffset = MWPaneFormatter.getUUIDStartOffset(textPane, element);
    int endOffset = MWPaneFormatter.getUUIDEndOffet(textPane, element);

    // Retrieve main attributes
    AttributeSet attributes = element.getAttributes();
    Object attrPage = attributes.getAttribute(MWPaneFormatter.ATTRIBUTE_PAGE);
    Object attrPageElement = attributes.getAttribute(MWPaneFormatter.ATTRIBUTE_PAGE_ELEMENT);
    Object attrTemplateMatcher = attributes.getAttribute(MWPaneFormatter.ATTRIBUTE_TEMPLATE_MATCHER);
    Object attrText = attributes.getAttribute(MWPaneFormatter.ATTRIBUTE_TEXT);

    Page page = (attrPage instanceof Page) ? (Page) attrPage : null;
    TemplateMatcher matcher = (attrTemplateMatcher instanceof TemplateMatcher) ?
        (TemplateMatcher) attrTemplateMatcher : null;
    MWPaneDisambiguationMenuCreator menu = new MWPaneDisambiguationMenuCreator();

    // Manage TemplateMatcher
    if (attrPageElement instanceof PageElementTemplate) {
      PageElementTemplate template = (PageElementTemplate) attrPageElement;

      String templateTitle = getWikipedia().getWikiConfiguration().getPageTitle(
          Namespace.TEMPLATE,
          template.getTemplateName());
      JPopupMenu popup = menu.createPopupMenu(templateTitle);
      if ((matcher != null) &&
          (matcher.getExplanation() != null) &&
          (matcher.getExplanation().length() > 0)) {
        menu.addDisabledText(popup, "→ " + matcher.getExplanation() + " ←");
      }
      menu.addCurrentChapter(popup, position, pageAnalysis);

      menu.addSeparator(popup);
      Page templatePage = DataManager.getPage(getWikipedia(), templateTitle, null, null, null);

      menu.addReplaceTemplate(
          getWikipedia(), popup, template, matcher,
          page, pageAnalysis.getPage(), element, textPane);
      menu.addAnalyze(getWikipedia(), popup, page);
      menu.addAnalyze(getWikipedia(), popup, templatePage);
      menu.addView(getWikipedia(), popup, page, true);
      menu.addView(getWikipedia(), popup, templatePage, true);
      menu.addDisambiguation(getWikipedia(), popup, page);
      menu.addItemReloadLinks(getWikipedia(), popup, page, getWindow());

      return popup;
    }

    if ((!(attrPage instanceof Page)) || (!(attrText instanceof String))) {
      return null;
    }

    // Menu name
    String text = (String) attrText;
    JPopupMenu popup = menu.createPopupMenu(null);

    // Create sub menus
    Configuration config = Configuration.getConfiguration();
    AbstractButton addNote = null;
    if (config.getBoolean(null, ConfigurationValueBoolean.ADD_NOTE_FOR_HELP)) {
      addNote = textPane.getCheckBoxAddNote();
    }
    menu.addReplaceLink(getWikipedia(), popup, page, text, element, textPane);
    menu.addItemRemoveLink(popup, text, textPane, startOffset, endOffset);
    menu.addMarkAsNormal(getWikipedia(), popup, page, text, element, textPane);
    menu.addMarkAsNeedingHelp(getWikipedia(), popup, page, text, element, textPane, addNote);
    menu.addLinkText(getWikipedia(), popup, page, text, element, textPane);
    menu.addSeparator(popup);
    if (page != null) {
      menu.addDisabledText(popup, page.getTitle());
    }
    menu.addCurrentChapter(popup, position, pageAnalysis);
    menu.addSeparator(popup);
    menu.addAnalyze(getWikipedia(), popup, page);
    menu.addView(getWikipedia(), popup, page, true);
    menu.addDisambiguation(getWikipedia(), popup, page);
    menu.addItemReloadLinks(getWikipedia(), popup, page, getWindow());

    return popup;
  }
}
