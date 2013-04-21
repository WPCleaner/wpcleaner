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

import javax.swing.JPopupMenu;
import javax.swing.text.AttributeSet;
import javax.swing.text.Element;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.PageAnalysis;
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
    return popup;
  }
}
