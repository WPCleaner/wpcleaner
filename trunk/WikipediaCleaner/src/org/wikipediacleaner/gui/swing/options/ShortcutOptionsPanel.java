/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.options;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.KeyEventDispatcher;
import java.awt.KeyboardFocusManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;


/**
 * A panel for shortcut options.
 */
public class ShortcutOptionsPanel extends OptionsPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = 5269712919862161621L;

  /**
   * Construct a Shortcut Options panel.
   */
  public ShortcutOptionsPanel() {
    super(new GridBagLayout());
    initialize();
  }

  private int lineAddToWatchList;
  private int lineApply;
  private int lineClose;
  private int lineDabAnalysis;
  private int lineExternalViewer;
  private int lineFullAnalysis;
  private int lineHistory;
  private int lineRestoreDefaults;
  private int lineValidate;

  /**
   * A notice to explain how to set up a shortcut.
   */
  private JLabel notice;

  /**
   * Initialize the panel.
   */
  private void initialize() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Shortcut options (restart needed)")));

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

    // Add line for add to watch list
    lineAddToWatchList = addLine(constraints, GT._("Add to Watch list"));
    setShortcut(lineAddToWatchList, ConfigurationValueShortcut.ADD_TO_WATCH_LIST);

    // Add line for apply
    lineApply = addLine(constraints, GT._("Apply"));
    setShortcut(lineApply, ConfigurationValueShortcut.APPLY);

    // Add line for close
    lineClose = addLine(constraints, GT._("Close"));
    setShortcut(lineClose, ConfigurationValueShortcut.CLOSE);

    // Add line for disambiguation analysis
    lineDabAnalysis = addLine(constraints, GT._("Disambiguation"));
    setShortcut(lineDabAnalysis, ConfigurationValueShortcut.DAB_ANALYSIS);

    // Add line for external viewer
    lineExternalViewer = addLine(constraints, GT._("External Viewer"));
    setShortcut(lineExternalViewer, ConfigurationValueShortcut.EXTERNAL_VIEWER);

    // Add line for full analysis
    lineFullAnalysis = addLine(constraints, GT._("Full analysis"));
    setShortcut(lineFullAnalysis, ConfigurationValueShortcut.FULL_ANALYSIS);

    // Add line for history
    lineHistory = addLine(constraints, GT._("History"));
    setShortcut(lineHistory, ConfigurationValueShortcut.HISTORY);

    // Add line for restore defaults
    lineRestoreDefaults = addLine(constraints, GT._("Restore defaults"));
    setShortcut(lineRestoreDefaults, ConfigurationValueShortcut.RESTORE_DEFAULTS);

    // Add line for validate
    lineValidate = addLine(constraints, GT._("Validate"));
    setShortcut(lineValidate, ConfigurationValueShortcut.VALIDATE);

    // Add a notice
    notice = new JLabel(GT._("Press key for shortcut or ESC to cancel"));
    notice.setForeground(Color.RED);
    notice.setVisible(false);
    notice.setHorizontalAlignment(SwingConstants.CENTER);
    constraints.gridx = 0;
    constraints.gridwidth = columnCount;
    add(notice, constraints);
    constraints.gridy++;

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
    setShortcut(lineAddToWatchList, ConfigurationValueShortcut.ADD_TO_WATCH_LIST);
    setShortcut(lineApply, ConfigurationValueShortcut.APPLY);
    setShortcut(lineClose, ConfigurationValueShortcut.CLOSE);
    setShortcut(lineDabAnalysis, ConfigurationValueShortcut.DAB_ANALYSIS);
    setShortcut(lineExternalViewer, ConfigurationValueShortcut.EXTERNAL_VIEWER);
    setShortcut(lineFullAnalysis, ConfigurationValueShortcut.FULL_ANALYSIS);
    setShortcut(lineHistory, ConfigurationValueShortcut.HISTORY);
    setShortcut(lineRestoreDefaults, ConfigurationValueShortcut.RESTORE_DEFAULTS);
    setShortcut(lineValidate, ConfigurationValueShortcut.VALIDATE);
  }

  /**
   * Apply new values to the options.
   */
  @Override
  public void apply() {
    super.apply();
    Configuration config = Configuration.getConfiguration();
    ConfigurationValueShortcut.ShortcutProperties configShortcut = null;

    configShortcut = getShortcut(lineAddToWatchList);
    config.setShortcut(ConfigurationValueShortcut.ADD_TO_WATCH_LIST, configShortcut);
    configShortcut = getShortcut(lineApply);
    config.setShortcut(ConfigurationValueShortcut.APPLY, configShortcut);
    configShortcut = getShortcut(lineClose);
    config.setShortcut(ConfigurationValueShortcut.CLOSE, configShortcut);
    configShortcut = getShortcut(lineDabAnalysis);
    config.setShortcut(ConfigurationValueShortcut.DAB_ANALYSIS, configShortcut);
    configShortcut = getShortcut(lineExternalViewer);
    config.setShortcut(ConfigurationValueShortcut.EXTERNAL_VIEWER, configShortcut);
    configShortcut = getShortcut(lineFullAnalysis);
    config.setShortcut(ConfigurationValueShortcut.FULL_ANALYSIS, configShortcut);
    configShortcut = getShortcut(lineHistory);
    config.setShortcut(ConfigurationValueShortcut.HISTORY, configShortcut);
    configShortcut = getShortcut(lineRestoreDefaults);
    config.setShortcut(ConfigurationValueShortcut.RESTORE_DEFAULTS, configShortcut);
    configShortcut = getShortcut(lineValidate);
    config.setShortcut(ConfigurationValueShortcut.VALIDATE, configShortcut);
  }

  // ==========================================================================
  // Managing the elements of a shortcut
  // ==========================================================================

  private int linesCount = 0;
  private final List<JCheckBox> chkEnabled = new ArrayList<JCheckBox>();
  private final List<JToggleButton> chkCtrl = new ArrayList<JToggleButton>();
  private final List<JToggleButton> chkAlt = new ArrayList<JToggleButton>();
  private final List<KeyCodeButton> btnKey = new ArrayList<KeyCodeButton>();

  // Count columns
  private final static int columnEnabled = 0;
  private final static int columnName = columnEnabled + 1;
  private final static int columnCtrl = columnName + 1;
  private final static int columnAlt = columnCtrl + 1;
  private final static int columnKey = columnAlt + 1;
  private final static int columnCount = columnKey + 1;

  /**
   * Add a line for a shortcut.
   * 
   * @param constraints Grid bag constraints.
   * @param name Descriptive name of the shortcut.
   * @return
   */
  private int addLine(
      GridBagConstraints constraints,
      String name) {

    // Enabled check box
    JCheckBox chk = Utilities.createJCheckBox("", true);
    chkEnabled.add(chk);
    constraints.gridx = columnEnabled;
    constraints.weightx = 0;
    add(chk, constraints);

    // Name
    if (name != null) {
      JLabel label = Utilities.createJLabel(name);
      constraints.gridx = columnName;
      constraints.weightx = 1;
      add(label, constraints);
    }

    // Ctrl key
    JToggleButton toggle = Utilities.createJToggleButton("CTRL");
    chkCtrl.add(toggle);
    constraints.gridx = columnCtrl;
    constraints.weightx = 0;
    add(toggle, constraints);

    // Alt key
    toggle = Utilities.createJToggleButton("ALT");
    chkAlt.add(toggle);
    constraints.gridx = columnAlt;
    constraints.weightx = 0;
    add(toggle, constraints);

    // Key
    KeyCodeButton button = new KeyCodeButton(0);
    btnKey.add(button);
    constraints.gridx = columnKey;
    constraints.weightx = 0;
    add(button, constraints);

    // Next line
    constraints.gridy++;
    int result = linesCount;
    linesCount++;
    return result;
  }

  /**
   * Set the attributes for a shortcut.
   * 
   * @param line Line number.
   * @param shortcut Shortcut.
   */
  private void setShortcut(int line, ConfigurationValueShortcut shortcut) {
    Configuration config = Configuration.getConfiguration();
    if (shortcut == null) {
      return;
    }
    ConfigurationValueShortcut.ShortcutProperties properties = config.getShortcut(shortcut);
    setEnabled(line, properties.getEnabled());
    setCtrl(line, properties.getCtrlKey());
    setAlt(line, properties.getAltKey());
    setKey(line, properties.getKey());
  }

  /**
   * @param line Line number.
   * @return Shortcut.
   */
  private ConfigurationValueShortcut.ShortcutProperties getShortcut(int line) {
    if ((line < 0) || (line >= linesCount)) {
      return null;
    }
    boolean enabled = true;
    if (chkEnabled.get(line) != null) {
      enabled = chkEnabled.get(line).isSelected();
    }
    boolean ctrl = false;
    if (chkCtrl.get(line) != null) {
      ctrl = chkCtrl.get(line).isSelected();
    }
    boolean alt = false;
    if (chkAlt.get(line) != null) {
      alt = chkAlt.get(line).isSelected();
    }
    int key = 0;
    if (btnKey.get(line) != null) {
      btnKey.get(line).getKeyCode();
    }
    return new ConfigurationValueShortcut.ShortcutProperties(
        enabled, ctrl, alt, key);
  }

  /**
   * Set the checkbox for the enabled column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setEnabled(int line, boolean value) {
    setToggleButton(chkEnabled, line, value);
  }

  /**
   * Set the checkbox for the ctrl column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setCtrl(int line, boolean value) {
    setToggleButton(chkCtrl, line, value);
  }

  /**
   * Set the checkbox for the alt column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setAlt(int line, boolean value) {
    setToggleButton(chkAlt, line, value);
  }

  /**
   * Set the value for the key column.
   * 
   * @param line Line number.
   * @param value Key value.
   */
  private void setKey(int line, int value) {
    if (btnKey == null) {
      return;
    }
    if ((line < 0) || (line >= btnKey.size())) {
      return;
    }
    KeyCodeButton btn = btnKey.get(line);
    if (btn == null) {
      return;
    }
    btn.setKeyCode(value);
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
   * Display a notice.
   * 
   * @param display True to display the notice.
   */
  void showNotice(boolean display) {
    notice.setVisible(display);
  }

  /**
   * Button for editing a key code.
   */
  private class KeyCodeButton extends JButton implements ActionListener, KeyEventDispatcher {

    /**
     * Serialization.
     */
    private static final long serialVersionUID = -2362841546820504549L;

    /**
     * Key code.
     */
    private int keyCode;

    /**
     * @param keyCode Key code.
     */
    KeyCodeButton(int keyCode) {
      super();
      setKeyCode(keyCode);
      addActionListener(this);
    }

    /**
     * @param keyCode Key code.
     */
    public void setKeyCode(int keyCode) {
      this.keyCode = keyCode;
      setText(KeyEvent.getKeyText(keyCode));
    }

    /**
     * @return Key code.
     */
    public int getKeyCode() {
      return keyCode;
    }

    /**
     * Action triggered when button is clicked.
     * 
     * @param e Event.
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
      showNotice(true);
      KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(this);
    }

    /**
     * Action triggered when keyboard is used.
     * 
     * @param e Event.
     * @return True if processing should end.
     * @see java.awt.KeyEventDispatcher#dispatchKeyEvent(java.awt.event.KeyEvent)
     */
    public boolean dispatchKeyEvent(KeyEvent e) {
      if (e.getKeyCode() != KeyEvent.VK_ESCAPE) {
        setKeyCode(e.getKeyCode());
      }
      showNotice(false);
      KeyboardFocusManager.getCurrentKeyboardFocusManager().removeKeyEventDispatcher(this);
      return false;
    }
  }
}
