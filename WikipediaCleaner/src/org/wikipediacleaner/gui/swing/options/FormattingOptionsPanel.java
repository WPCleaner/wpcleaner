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
import javax.swing.JSeparator;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueStyle;


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
    setStyle(lineComments, ConfigurationValueStyle.COMMENTS);

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
    setStyle(lineComments, ConfigurationValueStyle.COMMENTS);
  }

  /**
   * Apply new values to the options.
   */
  @Override
  public void apply() {
    Configuration config = Configuration.getConfiguration();
    ConfigurationValueStyle.StyleProperties configStyle = null;

    configStyle = getStyle(lineComments);
    config.setStyle(ConfigurationValueStyle.COMMENTS, configStyle);
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
  private final static int columnFormat = columnName + 1;
  private final static int columnCount = columnFormat + 1;

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

    // Toolbar
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    JToggleButton toggle = null;
    JCheckBox chk = null;
    JButton button = null;

    // Italic check box
    toggle = Utilities.createJToggleButton(
        "gnome-format-text-italic.png", EnumImageSize.SMALL,
        GT._("Italic"), false);
    toggle.setEnabled(italic);
    chkItalic.add(toggle);
    toolbar.add(toggle);

    // Bold check box
    toggle = Utilities.createJToggleButton(
        "gnome-format-text-bold.png", EnumImageSize.SMALL,
        GT._("Bold"), false);
    toggle.setEnabled(bold);
    chkBold.add(toggle);
    toolbar.add(toggle);

    // Underline check box
    toggle = Utilities.createJToggleButton(
        "gnome-format-text-underline.png", EnumImageSize.SMALL,
        GT._("Underline"), false);
    toggle.setEnabled(underline);
    chkUnderline.add(toggle);
    toolbar.add(toggle);

    // Strike through check box
    toggle = Utilities.createJToggleButton(
        "gnome-format-text-strikethrough.png", EnumImageSize.SMALL,
        GT._("Strike through"), false);
    toggle.setEnabled(strike);
    chkStrike.add(toggle);
    toolbar.add(toggle);

    // Foreground color check box and button
    toolbar.add(new JSeparator());
    chk = Utilities.createJCheckBox("", true);
    chk.setToolTipText(GT._("Foreground color"));
    chk.setEnabled(foreground);
    chkForeground.add(chk);
    toolbar.add(chk);
    button = Utilities.createJButton("    ");
    button.setToolTipText(GT._("Foreground color"));
    button.setEnabled(foreground && chk.isSelected());
    btnForeground.add(button);
    toolbar.add(button);

    // Background color check box and button
    toolbar.add(new JSeparator());
    chk = Utilities.createJCheckBox("", true);
    chk.setToolTipText(GT._("Background color"));
    chk.setEnabled(background);
    chkBackground.add(chk);
    toolbar.add(chk);
    button = Utilities.createJButton("    ");
    button.setToolTipText(GT._("Background color"));
    button.setEnabled(background && chk.isEnabled());
    btnBackground.add(button);
    toolbar.add(button);

    // End line
    constraints.gridx = columnFormat;
    constraints.weightx = 0;
    add(toolbar, constraints);

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
  private void setStyle(int line, ConfigurationValueStyle style) {
    Configuration config = Configuration.getConfiguration();
    if (style == null) {
      return;
    }
    ConfigurationValueStyle.StyleProperties properties = config.getStyle(style);
    setGeneral(line, properties.getEnabled());
    setItalic(line, properties.getItalic());
    setBold(line, properties.getBold());
    setUnderline(line, properties.getUnderline());
    setStrikeThrough(line, properties.getStrikeThrough());
    setForeground(line, properties.getForeground(), properties.getForegroundColor());
    setBackground(line, properties.getBackground(), properties.getBackgroundColor());
  }

  /**
   * @param line Line number.
   * @return Style.
   */
  private ConfigurationValueStyle.StyleProperties getStyle(int line) {
    if ((line < 0) || (line >= linesCount)) {
      return null;
    }
    boolean general = true;
    if (chkGeneral.get(line) != null) {
      general = chkGeneral.get(line).isSelected();
    }
    boolean italic = false;
    if (chkItalic.get(line) != null) {
      italic = chkItalic.get(line).isSelected();
    }
    boolean bold = false;
    if (chkBold.get(line) != null) {
      bold = chkBold.get(line).isSelected();
    }
    boolean underline = false;
    if (chkUnderline.get(line) != null) {
      underline = chkUnderline.get(line).isSelected();
    }
    boolean strike = false;
    if (chkStrike.get(line) != null) {
      strike = chkStrike.get(line).isSelected();
    }
    boolean foreground = false;
    if (chkForeground.get(line) != null) {
      foreground = chkForeground.get(line).isSelected();
    }
    Color foregroundColor = Color.BLACK;
    if (btnForeground.get(line) != null) {
      foregroundColor = btnForeground.get(line).getBackground();
    }
    boolean background = false;
    if (chkBackground.get(line) != null) {
      background = chkBackground.get(line).isSelected();
    }
    Color backgroundColor = Color.WHITE;
    if (btnBackground.get(line) != null) {
      backgroundColor = btnBackground.get(line).getBackground();
    }
    return new ConfigurationValueStyle.StyleProperties(
        general,
        foreground, foregroundColor,
        background, backgroundColor,
        italic, bold, underline, strike);
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
