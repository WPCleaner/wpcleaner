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

package org.wikipediacleaner.gui.swing.options;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;


/**
 * A panel for text formatting options.
 */
public class FormattingOptionsPanel extends OptionsPanel {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = -8809796439028282940L;

  /**
   * Construct a Formatting Options panel. 
   */
  public FormattingOptionsPanel() {
    super(new GridBagLayout());
    initialize();
  }

  private int lineComments;

  /**
   * Initialize the panel.
   */
  private void initialize() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Formatting options")));
    Configuration config = Configuration.getConfiguration();

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Add line for comments style
    lineComments = addLine(
        constraints, true, GT._("Comments"),
        true, true, true, true, true, true);
    setGeneral(lineComments, config.getBoolean(
        null,
        Configuration.BOOLEAN_COMMENTS_FMT,
        Configuration.DEFAULT_COMMENTS_FMT));
    setItalic(lineComments, config.getBoolean(
        null,
        Configuration.BOOLEAN_COMMENTS_FMT_ITALIC,
        Configuration.DEFAULT_COMMENTS_FMT_ITALIC));
    setBold(lineComments, config.getBoolean(
        null,
        Configuration.BOOLEAN_COMMENTS_FMT_BOLD,
        Configuration.DEFAULT_COMMENTS_FMT_BOLD));
    setUnderline(lineComments, config.getBoolean(
        null,
        Configuration.BOOLEAN_COMMENTS_FMT_UNDERLINE,
        Configuration.DEFAULT_COMMENTS_FMT_UNDERLINE));
    setStrikeThrough(lineComments, config.getBoolean(
        null,
        Configuration.BOOLEAN_COMMENTS_FMT_STRIKE,
        Configuration.DEFAULT_COMMENTS_FMT_STRIKE));
    setForeground(lineComments, config.getBoolean(
        null,
        Configuration.BOOLEAN_COMMENTS_FMT_FG,
        Configuration.DEFAULT_COMMENTS_FMT_FG));

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.gridwidth = columnCount;
    constraints.weighty = 1;
    add(emptyPanel, constraints);
  }

  /**
   * Restore all options to their default values.
   */
  @Override
  public void defaultValues() {
    // Comments
    setGeneral(lineComments, Configuration.DEFAULT_COMMENTS_FMT);
    setItalic(lineComments, Configuration.DEFAULT_COMMENTS_FMT_ITALIC);
    setBold(lineComments, Configuration.DEFAULT_COMMENTS_FMT_BOLD);
    setUnderline(lineComments, Configuration.DEFAULT_COMMENTS_FMT_UNDERLINE);
    setStrikeThrough(lineComments, Configuration.DEFAULT_COMMENTS_FMT_STRIKE);
    setForeground(lineComments, Configuration.DEFAULT_COMMENTS_FMT_FG);
  }

  /**
   * Apply new values to the options.
   */
  @Override
  public void apply() {
    Configuration config = Configuration.getConfiguration();
  }

  // ==========================================================================
  // Managing the formatting of a style
  // ==========================================================================

  private int linesCount = 0;
  private final List<JCheckBox> chkGeneral = new ArrayList<JCheckBox>();
  private final List<JToggleButton> chkItalic = new ArrayList<JToggleButton>();
  private final List<JToggleButton> chkBold = new ArrayList<JToggleButton>();
  private final List<JToggleButton> chkUnderline = new ArrayList<JToggleButton>();
  private final List<JToggleButton> chkStrike = new ArrayList<JToggleButton>();
  private final List<JCheckBox> chkForeground = new ArrayList<JCheckBox>();

  // Count columns
  private final static int columnGeneral = 0;
  private final static int columnName = columnGeneral + 1;
  private final static int columnItalic = columnName + 1;
  private final static int columnBold = columnItalic + 1;
  private final static int columnUnderline = columnBold + 1;
  private final static int columnStrikeThrough = columnUnderline + 1;
  private final static int columnForeground = columnStrikeThrough + 1;
  private final static int columnForegroundColor = columnForeground + 1;
  private final static int columnBackground = columnForegroundColor + 1;
  private final static int columnBackgroundColor = columnBackground + 1;
  private final static int columnCount = columnBackgroundColor + 1;

  /**
   * Add a line for a style.
   * 
   * @param constraints Grid bag constraints.
   * @param general Flag indicating if the style can be disabled.
   * @param name Descriptive name of the style.
   * @param italic Flag indicating if the style can be in italic.
   * @param bold Flag indicating if the style can be in bold.
   * @param underline Flag indicating it the style can be in underline.
   * @param strike Flag indicating if the style can be in strike through.
   * @param foreground Flag indicating if the foreground color can be changed.
   * @param background Flag indicating if the background color can be changed.
   * @return
   */
  private int addLine(
      GridBagConstraints constraints,
      boolean general, String name,
      boolean italic, boolean bold, boolean underline, boolean strike,
      boolean foreground, boolean background) {

    // General check box
    if (general) {
      JCheckBox chk = Utilities.createJCheckBox("", true);
      chkGeneral.add(chk);
      constraints.gridx = columnGeneral;
      constraints.weightx = 0;
      add(chk, constraints);
    } else {
      chkGeneral.add(null);
    }

    // Name
    if (name != null) {
      JLabel label = Utilities.createJLabel(name);
      constraints.gridx = columnName;
      constraints.weightx = 1;
      add(label, constraints);
    }

    // Italic check box
    if (italic) {
      JToggleButton toggle = Utilities.createJToggleButton(
          "gnome-format-text-italic.png", EnumImageSize.SMALL,
          GT._("Italic"), false);
      chkItalic.add(toggle);
      constraints.gridx = columnItalic;
      constraints.weightx = 0;
      add(toggle, constraints);
    } else {
      chkItalic.add(null);
    }

    // Bold check box
    if (bold) {
      JToggleButton toggle = Utilities.createJToggleButton(
          "gnome-format-text-bold.png", EnumImageSize.SMALL,
          GT._("Bold"), false);
      chkBold.add(toggle);
      constraints.gridx = columnBold;
      constraints.weightx = 0;
      add(toggle, constraints);
    } else {
      chkBold.add(null);
    }

    // Underline check box
    if (underline) {
      JToggleButton toggle = Utilities.createJToggleButton(
          "gnome-format-text-underline.png", EnumImageSize.SMALL,
          GT._("Underline"), false);
      chkUnderline.add(toggle);
      constraints.gridx = columnUnderline;
      constraints.weightx = 0;
      add(toggle, constraints);
    } else {
      chkUnderline.add(null);
    }

    // Strike through check box
    if (strike) {
      JToggleButton toggle = Utilities.createJToggleButton(
          "gnome-format-text-strikethrough.png", EnumImageSize.SMALL,
          GT._("Strike through"), false);
      chkStrike.add(toggle);
      constraints.gridx = columnStrikeThrough;
      constraints.weightx = 0;
      add(toggle, constraints);
    } else {
      chkStrike.add(null);
    }

    // Foreground color check box and button
    if (foreground) {
      JCheckBox chk = Utilities.createJCheckBox("", true);
      chk.setToolTipText(GT._("Foreground color"));
      chkForeground.add(chk);
      constraints.gridx = columnForeground;
      constraints.weightx = 0;
      add(chk, constraints);
    } else {
      chkForeground.add(null);
    }

    // Next line
    constraints.gridy++;
    int result = linesCount;
    linesCount++;
    return result;
  }

  /**
   * Set the checkbox for the general column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setGeneral(int line, boolean value) {
    setCheckBox(chkGeneral, line, value);
  }

  /**
   * Set the checkbox for the italic column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setItalic(int line, boolean value) {
    setCheckBox(chkItalic, line, value);
  }

  /**
   * Set the checkbox for the bold column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setBold(int line, boolean value) {
    setCheckBox(chkBold, line, value);
  }

  /**
   * Set the checkbox for the underline column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setUnderline(int line, boolean value) {
    setCheckBox(chkUnderline, line, value);
  }

  /**
   * Set the checkbox for the strike through column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setStrikeThrough(int line, boolean value) {
    setCheckBox(chkStrike, line, value);
  }

  /**
   * Set the checkbox for the foreground column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setForeground(int line, boolean value) {
    setCheckBox(chkForeground, line, value);
  }

  /**
   * Set the checkbox for a column.
   * 
   * @param list List of checkboxes.
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setCheckBox(List<? extends JToggleButton> list, int line, boolean value) {
    if (list == null) {
      return;
    }
    if ((line < 0) || (line >= list.size())) {
      return;
    }
    JToggleButton chk = list.get(line);
    if (chk == null) {
      return;
    }
    chk.setSelected(value);
  }
}
