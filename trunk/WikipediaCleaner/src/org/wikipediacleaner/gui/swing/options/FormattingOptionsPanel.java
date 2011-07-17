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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationStyle;


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
    ConfigurationStyle configStyle = null;

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
    configStyle = config.getStyle(
        Configuration.STYLE_COMMENTS_NAME,
        Configuration.STYLE_COMMENTS);
    setStyle(lineComments, configStyle);

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
    setStyle(lineComments, Configuration.STYLE_COMMENTS);
  }

  /**
   * Apply new values to the options.
   */
  @Override
  public void apply() {
    Configuration config = Configuration.getConfiguration();
    ConfigurationStyle configStyle = null;

    configStyle = getStyle(lineComments);
    config.setStyle(Configuration.STYLE_COMMENTS_NAME, configStyle);
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
  private final List<JButton> btnForeground = new ArrayList<JButton>();
  private final List<JCheckBox> chkBackground = new ArrayList<JCheckBox>();
  private final List<JButton> btnBackground = new ArrayList<JButton>();

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
      toggle.setBorderPainted(false);
      toggle.setFocusPainted(false);
      toggle.setMargin(new Insets(2, 2, 2, 2));
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
      toggle.setBorderPainted(false);
      toggle.setFocusPainted(false);
      toggle.setMargin(new Insets(2, 2, 2, 2));
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
      toggle.setBorderPainted(false);
      toggle.setFocusPainted(false);
      toggle.setMargin(new Insets(2, 2, 2, 2));
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
      toggle.setBorderPainted(false);
      toggle.setFocusPainted(false);
      toggle.setMargin(new Insets(2, 2, 2, 2));
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

      JButton button = Utilities.createJButton(" ");
      button.setToolTipText(GT._("Foreground color"));
      btnForeground.add(button);
      constraints.gridx = columnForegroundColor;
      add(button, constraints);
    } else {
      chkForeground.add(null);
      btnForeground.add(null);
    }

    // Background color check box and button
    if (background) {
      JCheckBox chk = Utilities.createJCheckBox("", true);
      chk.setToolTipText(GT._("Background color"));
      chkBackground.add(chk);
      constraints.gridx = columnBackground;
      constraints.weightx = 0;
      add(chk, constraints);

      JButton button = Utilities.createJButton(" ");
      button.setToolTipText(GT._("Background color"));
      btnBackground.add(button);
      constraints.gridx = columnBackgroundColor;
      add(button, constraints);
    } else {
      chkBackground.add(null);
      btnBackground.add(null);
    }

    // Next line
    constraints.gridy++;
    int result = linesCount;
    linesCount++;
    return result;
  }

  /**
   * Set the attributes for the style.
   * 
   * @param line Line number.
   * @param style Style.
   */
  private void setStyle(int line, ConfigurationStyle style) {
    if (style == null) {
      return;
    }
    setGeneral(line, style.getEnabled());
    setItalic(line, style.getItalic());
    setBold(line, style.getBold());
    setUnderline(line, style.getUnderline());
    setStrikeThrough(line, style.getStrikeThrough());
    setForeground(line, style.getForeground(), style.getForegroundValue());
    setBackground(line, style.getBackground(), style.getBackgroundValue());
  }

  /**
   * @param line Line number.
   * @return Style.
   */
  private ConfigurationStyle getStyle(int line) {
    if ((line < 0) || (line >= linesCount)) {
      return null;
    }
    ConfigurationStyle result = new ConfigurationStyle();
    if (chkGeneral.get(line) != null) {
      result.setEnabled(chkGeneral.get(line).isSelected());
    }
    if (chkItalic.get(line) != null) {
      result.setItalic(chkItalic.get(line).isSelected());
    }
    if (chkBold.get(line) != null) {
      result.setBold(chkBold.get(line).isSelected());
    }
    if (chkUnderline.get(line) != null) {
      result.setUnderline(chkUnderline.get(line).isSelected());
    }
    if (chkStrike.get(line) != null) {
      result.setStrikeThrough(chkStrike.get(line).isSelected());
    }
    if (chkForeground.get(line) != null) {
      result.setForeground(chkForeground.get(line).isSelected());
    }
    if (btnForeground.get(line) != null) {
      result.setForegroundValue(btnForeground.get(line).getBackground());
    }
    if (chkBackground.get(line) != null) {
      result.setBackground(chkBackground.get(line).isSelected());
    }
    if (btnBackground.get(line) != null) {
      result.setBackgroundValue(btnBackground.get(line).getBackground());
    }
    return result;
  }

  /**
   * Set the checkbox for the general column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setGeneral(int line, boolean value) {
    setToggleButton(chkGeneral, line, value);
  }

  /**
   * Set the checkbox for the italic column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setItalic(int line, boolean value) {
    setToggleButton(chkItalic, line, value);
  }

  /**
   * Set the checkbox for the bold column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setBold(int line, boolean value) {
    setToggleButton(chkBold, line, value);
  }

  /**
   * Set the checkbox for the underline column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setUnderline(int line, boolean value) {
    setToggleButton(chkUnderline, line, value);
  }

  /**
   * Set the checkbox for the strike through column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setStrikeThrough(int line, boolean value) {
    setToggleButton(chkStrike, line, value);
  }

  /**
   * Set the checkbox and the button for the foreground column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   * @param color Color value.
   */
  private void setForeground(int line, boolean value, Color color) {
    setToggleButton(chkForeground, line, value);
    setColor(btnForeground, line, color);
  }

  /**
   * Set the checkbox and the button for the background column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   * @param color Color value.
   */
  private void setBackground(int line, boolean value, Color color) {
    setToggleButton(chkBackground, line, value);
    setColor(btnBackground, line, color);
  }

  /**
   * Set the toggle button for a column.
   * 
   * @param list List of checkboxes.
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setToggleButton(List<? extends JToggleButton> list, int line, boolean value) {
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

  /**
   * Set the color of a button for a column.
   * 
   * @param list List of buttons.
   * @param line Line number.
   * @param color Color value.
   */
  private void setColor(List<JButton> list, int line, Color color) {
    if (list == null) {
      return;
    }
    if ((line < 0) || (line >= list.size())) {
      return;
    }
    JButton button = list.get(line);
    if (button == null) {
      return;
    }
    button.setBackground(color);
  }
}
