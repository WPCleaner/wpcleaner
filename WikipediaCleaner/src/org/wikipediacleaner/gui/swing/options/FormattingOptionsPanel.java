/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.options;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.ColorButton;
import org.wikipediacleaner.gui.swing.options.OptionsPanel;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;
import org.wikipediacleaner.utils.ConfigurationValueStyle;


/**
 * A panel for text formatting options.
 */
public class FormattingOptionsPanel extends OptionsPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -8809796439028282940L;

  /**
   * Construct a Formatting Options panel. 
   */
  public FormattingOptionsPanel() {
    super(new GridBagLayout());
    initialize();
  }

  /**
   * Styles
   */
  private Vector<ConfigurationValueStyle> styles;

  /**
   * Initialize the panel.
   */
  private void initialize() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._T("Formatting options (restart needed)")));

    styles = new Vector<ConfigurationValueStyle>();

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

    // Create internal panel
    JPanel panel = new JPanel(new GridBagLayout());
    JScrollPane scrollPane = new JScrollPane(
        panel,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    scrollPane.setMinimumSize(new Dimension(200, 200));
    scrollPane.setPreferredSize(new Dimension(300, 400));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 1;
    constraints.weighty = 1;
    add(scrollPane, constraints);

    // Panel for first options
    JPanel firstPanel = new JPanel(new GridBagLayout());

    // Add font name
    Vector<String> fonts = new Vector<String>();
    for (Font font : GraphicsEnvironment.getLocalGraphicsEnvironment().getAllFonts()) {
      fonts.addElement(font.getName());
    }
    JComboBox<String> cmbFontName = createJComboBox(ConfigurationValueString.EDITOR_FONT_NAME, fonts);
    JLabel lblFontName = Utilities.createJLabel(GT._T("Select editor font name"));
    lblFontName.setLabelFor(cmbFontName);
    lblFontName.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.gridwidth = 1;
    constraints.weightx = 0;
    firstPanel.add(lblFontName, constraints);
    constraints.gridx = 1;
    constraints.gridwidth = 1;
    constraints.weightx = 1;
    firstPanel.add(cmbFontName, constraints);
    constraints.gridy++;


    // Add font size
    JSpinner spinFontSize = createJSpinner(ConfigurationValueInteger.EDITOR_FONT_SIZE, 8, 72, 1);
    JLabel lblFontSize = Utilities.createJLabel(GT._T("Select editor font size"));
    lblFontSize.setLabelFor(spinFontSize);
    lblFontSize.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.gridwidth = 1;
    constraints.weightx = 0;
    firstPanel.add(lblFontSize, constraints);
    constraints.gridx = 1;
    constraints.gridwidth = 1;
    constraints.weightx = 1;
    firstPanel.add(spinFontSize, constraints);
    constraints.gridy++;

    // Add font size increase
    JSpinner spinFontIncrease = createJSpinner(ConfigurationValueInteger.FONT_SIZE, 0, 20, 1);
    JLabel lblFontIncrease = Utilities.createJLabel(GT._T("Increase font size"));
    lblFontIncrease.setLabelFor(spinFontIncrease);
    lblFontIncrease.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.gridwidth = 1;
    constraints.weightx = 0;
    firstPanel.add(lblFontIncrease, constraints);
    constraints.gridx = 1;
    constraints.gridwidth = 1;
    constraints.weightx = 1;
    firstPanel.add(spinFontIncrease, constraints);
    constraints.gridy++;

    // Add first options in the first panel
    constraints.gridx = columnName;
    constraints.gridwidth = columnCount - columnName;
    constraints.weightx = 1;
    constraints.gridy = 0;
    panel.add(firstPanel, constraints);
    constraints.gridwidth = 1;
    constraints.gridy++;

    // Add line for comments style
    addLine(
        panel, constraints, true, GT._T("Comments"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.COMMENTS);

    // Add line for internal link style
    addLine(
        panel, constraints, true, GT._T("Internal link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.INTERNAL_LINK);

    // Add line for internal link style (for redirect link)
    addLine(
        panel, constraints, true, GT._T("Redirect link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.INTERNAL_LINK_DEFAULT_REDIRECT);

    // Add line for internal link style (for missing link)
    addLine(
        panel, constraints, true, GT._T("Missing link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.INTERNAL_LINK_DEFAULT_MISSING);

    // Add line for template style
    addLine(
        panel, constraints, true, GT._T("Template"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.TEMPLATE);

    // Add line for title style
    addLine(
        panel, constraints, true, GT._T("Title"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.TITLE);

    // Add line for image style
    addLine(
        panel, constraints, true, GT._T("Image"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.IMAGE);

    // Add line for category style
    addLine(
        panel, constraints, true, GT._T("Category"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.CATEGORY);

    // Add line for DEFAULTSORT style
    addLine(
        panel, constraints, true, GT._T("Default sort"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.DEFAULTSORT);

    // Add line for language link style
    addLine(
        panel, constraints, true, GT._T("Language link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.LANGUAGE_LINK);

    // Add line for external link style
    addLine(
        panel, constraints, true, GT._T("External link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.EXTERNAL_LINK);

    // Add line for interwiki link style
    addLine(
        panel, constraints, true, GT._T("Interwiki link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.INTERWIKI_LINK);

    // Add line for tag style
    addLine(
        panel, constraints, true, GT._T("Tag"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.TAG);

    // Add line for list items style
    addLine(
        panel, constraints, true, GT._T("List items"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.LIST_ITEM);

    // Add line for table style
    addLine(
        panel, constraints, true, GT._T("Table"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.TABLE);

    // Add line for reference style
    addLine(
        panel, constraints, true, GT._T("Reference"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.REFERENCE);

    // Add line for programming elements style
    addLine(
        panel, constraints, true, GT._T("Programming elements"), 
        true, true, true, true, true, true,
        ConfigurationValueStyle.PROGRAMMING);

    // Add line for disambiguation link style
    addLine(
        panel, constraints, false, GT._T("Disambiguation link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.INTERNAL_LINK_DAB);

    // Add line for normal internal link style
    addLine(
        panel, constraints, false, GT._T("Normal internal link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.INTERNAL_LINK_NORMAL);

    // Add line for redirect link style
    addLine(
        panel, constraints, false, GT._T("Redirect link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.INTERNAL_LINK_REDIRECT);

    // Add line for missing link style
    addLine(
        panel, constraints, false, GT._T("Missing link"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.INTERNAL_LINK_MISSING);

    // Add line for disambiguation template style
    addLine(
        panel, constraints, false, GT._T("Disambiguation template"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.TEMPLATE_DAB);

    // Add line for normal template style
    addLine(
        panel, constraints, false, GT._T("Normal template"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.TEMPLATE_NORMAL);

    // Add line for help requested style
    addLine(
        panel, constraints, false, GT._T("Help requested"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.HELP_REQUESTED);

    // Add line for check wiki error style
    addLine(
        panel, constraints, false, GT._T("Check wiki error"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.CHECK_WIKI_ERROR);

    // Add line for check wiki warning style
    addLine(
        panel, constraints, false, GT._T("Check wiki warning"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.CHECK_WIKI_WARNING);

    // Add line for check wiki ok style
    addLine(
        panel, constraints, false, GT._T("Check wiki OK"),
        true, true, true, true, true, true,
        ConfigurationValueStyle.CHECK_WIKI_OK);

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.gridx = 0;
    constraints.gridwidth = columnCount;
    constraints.weighty = 1;
    add(emptyPanel, constraints);
  }

  /**
   * Restore all options to their default values.
   */
  @Override
  public void defaultValues() {
    super.defaultValues();
    for (int line = 0; line < styles.size(); line++) {
      setStyle(line, styles.get(line));
    }
  }

  /**
   * Apply new values to the options.
   */
  @Override
  public void apply() {
    super.apply();
    Configuration config = Configuration.getConfiguration();
    for (int line = 0; line < styles.size(); line++) {
      ConfigurationValueStyle.StyleProperties configStyle = getStyle(line);
      config.setStyle(styles.get(line), configStyle);
    }
  }

  // ==========================================================================
  // Managing the formatting of a style
  // ==========================================================================

  private final List<JCheckBox> chkGeneral = new ArrayList<JCheckBox>();
  private final List<JToggleButton> chkItalic = new ArrayList<JToggleButton>();
  private final List<JToggleButton> chkBold = new ArrayList<JToggleButton>();
  private final List<JToggleButton> chkUnderline = new ArrayList<JToggleButton>();
  private final List<JToggleButton> chkStrike = new ArrayList<JToggleButton>();
  private final List<JCheckBox> chkForeground = new ArrayList<JCheckBox>();
  private final List<ColorButton> btnForeground = new ArrayList<ColorButton>();
  private final List<JCheckBox> chkBackground = new ArrayList<JCheckBox>();
  private final List<ColorButton> btnBackground = new ArrayList<ColorButton>();

  // Count columns
  private final static int columnGeneral = 0;
  private final static int columnName = columnGeneral + 1;
  private final static int columnFormat = columnName + 1;
  private final static int columnCount = columnFormat + 1;

  /**
   * Add a line for a style.
   * 
   * @param panel Panel containing the styles.
   * @param constraints Grid bag constraints.
   * @param general Flag indicating if the style can be disabled.
   * @param name Descriptive name of the style.
   * @param italic Flag indicating if the style can be in italic.
   * @param bold Flag indicating if the style can be in bold.
   * @param underline Flag indicating it the style can be in underline.
   * @param strike Flag indicating if the style can be in strike through.
   * @param foreground Flag indicating if the foreground color can be changed.
   * @param background Flag indicating if the background color can be changed.
   * @param style Style.
   */
  private void addLine(
      JPanel panel, GridBagConstraints constraints,
      boolean general, String name,
      boolean italic, boolean bold, boolean underline, boolean strike,
      boolean foreground, boolean background,
      ConfigurationValueStyle style) {

    // General check box
    if (general) {
      JCheckBox chk = Utilities.createJCheckBox("", true);
      chkGeneral.add(chk);
      constraints.gridx = columnGeneral;
      constraints.weightx = 0;
      panel.add(chk, constraints);
    } else {
      chkGeneral.add(null);
    }

    // Name
    if (name != null) {
      JLabel label = Utilities.createJLabel(name);
      constraints.gridx = columnName;
      constraints.weightx = 1;
      panel.add(label, constraints);
    }

    // Toolbar
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    JToggleButton toggle = null;
    JCheckBox chk = null;
    ColorButton button = null;

    // Italic check box
    toggle = Utilities.createJToggleButton(
        "gnome-format-text-italic.png", EnumImageSize.SMALL,
        GT._T("Italic"), false);
    toggle.setEnabled(italic);
    chkItalic.add(toggle);
    toolbar.add(toggle);

    // Bold check box
    toggle = Utilities.createJToggleButton(
        "gnome-format-text-bold.png", EnumImageSize.SMALL,
        GT._T("Bold"), false);
    toggle.setEnabled(bold);
    chkBold.add(toggle);
    toolbar.add(toggle);

    // Underline check box
    toggle = Utilities.createJToggleButton(
        "gnome-format-text-underline.png", EnumImageSize.SMALL,
        GT._T("Underline"), false);
    toggle.setEnabled(underline);
    chkUnderline.add(toggle);
    toolbar.add(toggle);

    // Strike through check box
    toggle = Utilities.createJToggleButton(
        "gnome-format-text-strikethrough.png", EnumImageSize.SMALL,
        GT._T("Strike through"), false);
    toggle.setEnabled(strike);
    chkStrike.add(toggle);
    toolbar.add(toggle);

    // Foreground color check box and button
    toolbar.add(new JSeparator());
    chk = Utilities.createJCheckBox("", true);
    chk.setToolTipText(GT._T("Foreground color"));
    chk.setEnabled(foreground);
    chkForeground.add(chk);
    toolbar.add(chk);
    button = new ColorButton(Color.BLACK, GT._T("Choose foreground color"));
    button.setToolTipText(GT._T("Foreground color"));
    button.setEnabled(foreground && chk.isSelected());
    btnForeground.add(button);
    toolbar.add(button);

    // Background color check box and button
    toolbar.add(new JSeparator());
    chk = Utilities.createJCheckBox("", true);
    chk.setToolTipText(GT._T("Background color"));
    chk.setEnabled(background);
    chkBackground.add(chk);
    toolbar.add(chk);
    button = new ColorButton(Color.WHITE, GT._T("Choose background color"));
    button.setToolTipText(GT._T("Background color"));
    button.setEnabled(background && chk.isEnabled());
    btnBackground.add(button);
    toolbar.add(button);

    // End line
    constraints.gridx = columnFormat;
    constraints.weightx = 0;
    panel.add(toolbar, constraints);

    // Next line
    constraints.gridy++;
    int line = styles.size();
    styles.add(style);
    setStyle(line, style);
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
    if ((line < 0) || (line >= styles.size())) {
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
  private void setColor(List<ColorButton> list, int line, Color color) {
    if (list == null) {
      return;
    }
    if ((line < 0) || (line >= list.size())) {
      return;
    }
    ColorButton button = list.get(line);
    if (button == null) {
      return;
    }
    button.setColor(color);
  }
}
