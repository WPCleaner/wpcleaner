/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.font.TextAttribute;
import java.util.HashMap;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.border.EmptyBorder;

import org.wikipediacleaner.api.data.Page;


/**
 * 
 */
public class PageListCellRenderer extends JLabel implements ListCellRenderer {

  private static final long serialVersionUID = 1456336109709806845L;

  private boolean showCountOccurence;

  private Font missingFont;
  private Font normalFont;
  private Font redirectFont;

  public PageListCellRenderer() {
    setOpaque(true);
    setHorizontalAlignment(LEFT);
    setVerticalAlignment(CENTER);
    setBorder(new EmptyBorder(0, 3, 0, 3));
    HashMap<TextAttribute, Boolean> missingAttributes = new HashMap<TextAttribute, Boolean>();
    missingAttributes.put(TextAttribute.STRIKETHROUGH, Boolean.TRUE /*TextAttribute.STRIKETHROUGH_ON*/);
    missingFont = getFont().deriveFont(missingAttributes);
    normalFont = getFont().deriveFont(Font.PLAIN);
    redirectFont = getFont().deriveFont(Font.ITALIC);
    setFont(normalFont);
  }

  /**
   * @param show Flag indicating the occurence count is shown.
   */
  public void showCountOccurence(boolean show) {
    showCountOccurence = show;
  }

  /* (non-Javadoc)
   * @see javax.swing.ListCellRenderer#getListCellRendererComponent(
   *          javax.swing.JList, java.lang.Object, int, boolean, boolean)
   */
  public Component getListCellRendererComponent(
      JList list,
      Object value,
      @SuppressWarnings("unused") int index,
      boolean isSelected,
      @SuppressWarnings("unused") boolean cellHasFocus) {

    // Retrieve data
    String text = (value != null) ? value.toString() : "";
    Boolean disambiguation = null;
    Boolean exist = null;
    boolean redirect = false;
    if (value instanceof Page) {
      Page page = (Page) value;
      text = page.getTitle();
      disambiguation = page.isDisambiguationPage();
      exist = page.isExisting();
      if (showCountOccurence && (page.getCountOccurence() > 0)) {
        text += " => " + page.getCountOccurence(); 
      }
      redirect = page.isRedirect();
    }

    // Text
    setText(text);

    // Color
    Color background = isSelected ? list.getSelectionBackground() : list.getBackground();
    Color foreground = isSelected ? list.getSelectionForeground() : list.getForeground();
    if (disambiguation == null) {
      if (!isSelected) {
        foreground = Color.DARK_GRAY;
      }
    } else if (disambiguation.booleanValue()) {
      foreground = Color.RED;
    }
    setBackground(background);
    setForeground(foreground);

    // Font
    if (Boolean.FALSE.equals(exist)) {
      setFont(missingFont);
    } else {
      setFont(redirect ? redirectFont : normalFont);
    }

    return this;
  }

}
