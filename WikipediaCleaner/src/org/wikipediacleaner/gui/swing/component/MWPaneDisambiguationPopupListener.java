/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing.component;

import javax.swing.JCheckBox;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.text.AttributeSet;
import javax.swing.text.Element;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.TemplateMatcher;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


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

    // Manage TemplateMatcher
    if (attrPageElement instanceof PageElementTemplate) {
      PageElementTemplate template = (PageElementTemplate) attrPageElement;
      JPopupMenu popup = new JPopupMenu();

      String templateTitle = getWikipedia().getWikiConfiguration().getPageTitle(
          Namespace.TEMPLATE,
          template.getTemplateName());
      JMenuItem menuItem = new JMenuItem(templateTitle);
      menuItem.setEnabled(false);
      popup.add(menuItem);
      if ((matcher != null) &&
          (matcher.getExplanation() != null) &&
          (matcher.getExplanation().length() > 0)) {
        menuItem = new JMenuItem("=> " + matcher.getExplanation() + " <=");
        menuItem.setEnabled(false);
        popup.add(menuItem);
      }
      MenuCreator.addCurrentChapterToMenu(popup, position, pageAnalysis);

      popup.addSeparator();
      Page templatePage = DataManager.getPage(getWikipedia(), templateTitle, null, null);

      MenuCreator.addReplaceTemplateToMenu(
          getWikipedia(), popup, template, matcher,
          page, pageAnalysis.getPage(), element, textPane);
      MenuCreator.addAnalyzeToMenu(getWikipedia(), popup, page);
      MenuCreator.addAnalyzeToMenu(getWikipedia(), popup, templatePage);
      MenuCreator.addViewToMenu(getWikipedia(), popup, page, true);
      MenuCreator.addViewToMenu(getWikipedia(), popup, templatePage, true);
      MenuCreator.addDisambiguationToMenu(getWikipedia(), popup, page);
      MenuCreator.addReloadLinksToMenu(getWikipedia(), popup, page, getWindow());

      return popup;
    }

    if ((!(attrPage instanceof Page)) || (!(attrText instanceof String))) {
      return null;
    }

    // Menu name
    String text = (String) attrText;
    JPopupMenu popup = new JPopupMenu();

    // Create sub menus
    JCheckBox chk = null;
    JCheckBox createDab = textPane.getCheckBoxCreateDabWarning();
    JCheckBox updateDab = textPane.getCheckBoxUpdateDabWarning();
    JCheckBox addNote = textPane.getCheckBoxAddNote();
    if ((createDab != null) && (createDab.isEnabled())) {
      chk = createDab;
    } else if ((updateDab != null) && (updateDab.isEnabled())) {
      chk = updateDab;
    } else if ((addNote != null) && (addNote.isEnabled())) {
      chk = addNote;
    }
    MenuCreator.addReplaceLinkToMenu(popup, page, text, element, textPane);
    MenuCreator.addRemoveLinkToMenu(popup, text, textPane, startOffset, endOffset);
    MenuCreator.addMarkAsNormalToMenu(getWikipedia(), popup, page, text, element, textPane);
    MenuCreator.addMarkAsNeedingHelpToMenu(getWikipedia(), popup, page, text, element, textPane, chk);
    MenuCreator.addLinkTextToMenu(getWikipedia(), popup, page, text, element, textPane);
    popup.add(new JSeparator());
    if (page != null) {
      JMenuItem menuItem = new JMenuItem(page.getTitle());
      menuItem.setEnabled(false);
      popup.add(menuItem);
    }
    MenuCreator.addCurrentChapterToMenu(popup, position, pageAnalysis);
    popup.add(new JSeparator());
    MenuCreator.addAnalyzeToMenu(getWikipedia(), popup, page);
    MenuCreator.addViewToMenu(getWikipedia(), popup, page, true);
    MenuCreator.addDisambiguationToMenu(getWikipedia(), popup, page);
    MenuCreator.addReloadLinksToMenu(getWikipedia(), popup, page, getWindow());

    return popup;
  }
}
