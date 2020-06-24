/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import javax.swing.JPopupMenu;
import javax.swing.text.AttributeSet;
import javax.swing.text.Element;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.menu.MWPaneCheckWikiMenuCreator;


/**
 * A popup menu listener for MediaWikiPane for Check Wiki.
 */
public class MWPaneCheckWikiPopupListener extends MWPanePopupListener {

  public MWPaneCheckWikiPopupListener(
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

    // Check if it's for Check Wiki
    AttributeSet attributes = element.getAttributes();
    Object attrInfo = attributes.getAttribute(MWPaneFormatter.ATTRIBUTE_INFO);
    if (!(attrInfo instanceof CheckErrorResult)) {
      return null;
    }

    CheckErrorResult info = (CheckErrorResult) attrInfo;
    MWPaneCheckWikiMenuCreator menu = new MWPaneCheckWikiMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(null);
    menu.addInfo(popup, element, textPane, info);
    menu.addItemCopyPaste(popup, textPane);
    return popup;
  }
}
