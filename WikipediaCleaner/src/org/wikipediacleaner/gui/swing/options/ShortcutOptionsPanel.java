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
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToggleButton;
import javax.swing.ScrollPaneConstants;
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

  /**
   * Shortcuts
   */
  private Vector<ConfigurationValueShortcut> shortcuts;

  /**
   * A notice to explain how to set up a shortcut.
   */
  private JLabel notice;

  /**
   * Initialize the panel.
   */
  private void initialize() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._T("Shortcut options (restart needed)")));

    shortcuts = new Vector<ConfigurationValueShortcut>();

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

    // Add line for add to watch list
    addLine(
        panel, constraints, GT._T("Add to local watchlist"),
        ConfigurationValueShortcut.ADD_TO_WATCH_LIST);

    // Add line for apply
    addLine(
        panel, constraints, GT._T("Apply"),
        ConfigurationValueShortcut.APPLY);

    // Add line for bug report
    addLine(
        panel, constraints, GT._T("Idea? Bug?"),
        ConfigurationValueShortcut.BUG_REPORT);

    // Add line for close
    addLine(
        panel, constraints, GT._T("Close"),
        ConfigurationValueShortcut.CLOSE);

    // Add line for current disambiguation list
    addLine(
        panel, constraints, GT._T("Current disambiguation list"),
        ConfigurationValueShortcut.CURRENT_DAB_LIST);

    // Add line for disambiguation analysis
    addLine(
        panel, constraints, GT._T("Disambiguation"),
        ConfigurationValueShortcut.DAB_ANALYSIS);

    // Add line for external viewer
    addLine(
        panel, constraints, GT._T("External Viewer"),
        ConfigurationValueShortcut.EXTERNAL_VIEWER);

    // Add line for full analysis
    addLine(
        panel, constraints, GT._T("Full analysis"),
        ConfigurationValueShortcut.FULL_ANALYSIS);

    // Add line for help
    addLine(
        panel, constraints, GT._T("Help"),
        ConfigurationValueShortcut.HELP);

    // Add line for history
    addLine(
        panel, constraints, GT._T("History"),
        ConfigurationValueShortcut.HISTORY);

    // Add line for login
    addLine(
        panel, constraints, GT._T("Login"),
        ConfigurationValueShortcut.LOGIN);

    // Add line for logout
    addLine(
        panel, constraints, GT._T("Logout"),
        ConfigurationValueShortcut.LOGOUT);

    // Add line for options
    addLine(
        panel, constraints, GT._T("Options"),
        ConfigurationValueShortcut.OPTIONS);

    // Add line for random page
    addLine(
        panel, constraints, GT._T("Random page"),
        ConfigurationValueShortcut.RANDOM_PAGE);

    // Add line for restore defaults
    addLine(
        panel, constraints, GT._T("Restore defaults"),
        ConfigurationValueShortcut.RESTORE_DEFAULTS);

    // Add line for send
    addLine(
        panel, constraints, GT._T("Send"),
        ConfigurationValueShortcut.SEND);

    // Add line for system options
    addLine(
        panel, constraints, GT._T("System options"),
        ConfigurationValueShortcut.SYSTEM_OPTIONS);

    // Add line for validate
    addLine(
        panel, constraints, GT._T("Validate"),
        ConfigurationValueShortcut.VALIDATE);

    // Add line for watch list
    addLine(
        panel, constraints, GT._T("Local Watchlist"),
        ConfigurationValueShortcut.WATCH_LIST);

    // Add line for adding to list
    addLine(
        panel, constraints, GT._T("Add to list"),
        ConfigurationValueShortcut.LIST_ADD);

    // Add line for removing from list
    addLine(
        panel, constraints, GT._T("Remove from list"),
        ConfigurationValueShortcut.LIST_REMOVE);

    // Add line for first occurrence
    addLine(
        panel, constraints, GT._T("First occurrence"),
        ConfigurationValueShortcut.OCCURRENCE_FIRST);

    // Add line for previous occurrence
    addLine(
        panel, constraints, GT._T("Previous occurrence"),
        ConfigurationValueShortcut.OCCURRENCE_PREVIOUS);

    // Add line for next occurrence
    addLine(
        panel, constraints, GT._T("Next occurrence"),
        ConfigurationValueShortcut.OCCURRENCE_NEXT);

    // Add line for last occurrence
    addLine(
        panel, constraints, GT._T("Last occurrence"),
        ConfigurationValueShortcut.OCCURRENCE_LAST);

    // Add a notice
    notice = new JLabel(GT._T("Press key for shortcut or ESC to cancel"));
    notice.setForeground(Color.RED);
    notice.setVisible(false);
    notice.setHorizontalAlignment(SwingConstants.CENTER);
    constraints.gridx = 0;
    constraints.gridwidth = columnCount;
    panel.add(notice, constraints);
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
    panel.add(emptyPanel, constraints);
  }

  /**
   * Restore all options to their default values.
   */
  @Override
  public void defaultValues() {
    super.defaultValues();
    for (int line = 0; line < shortcuts.size(); line++) {
      setShortcut(line, shortcuts.get(line));
    }
  }

  /**
   * Apply new values to the options.
   */
  @Override
  public void apply() {
    super.apply();
    for (int line = 0; line < shortcuts.size(); line++) {
      applyShortcut(shortcuts.get(line), line);
    }
  }

  /**
   * Apply new values for a shortcut.
   * 
   * @param shortcut Shortcut.
   * @param line Line.
   */
  private void applyShortcut(ConfigurationValueShortcut shortcut, int line) {
    Configuration config = Configuration.getConfiguration();
    ConfigurationValueShortcut.ShortcutProperties configShortcut = getShortcut(line, shortcut.getName());
    config.setShortcut(shortcut, configShortcut);
  }

  // ==========================================================================
  // Managing the elements of a shortcut
  // ==========================================================================

  private final List<JCheckBox> chkEnabled = new ArrayList<JCheckBox>();
  private final List<JToggleButton> chkCtrl = new ArrayList<JToggleButton>();
  private final List<JToggleButton> chkAlt = new ArrayList<JToggleButton>();
  private final List<JToggleButton> chkShift = new ArrayList<JToggleButton>();
  private final List<KeyCodeButton> btnKey = new ArrayList<KeyCodeButton>();

  // Count columns
  private final static int columnEnabled = 0;
  private final static int columnName = columnEnabled + 1;
  private final static int columnCtrl = columnName + 1;
  private final static int columnAlt = columnCtrl + 1;
  private final static int columnShift = columnAlt + 1;
  private final static int columnKey = columnShift + 1;
  private final static int columnCount = columnKey + 1;

  /**
   * Add a line for a shortcut.
   * 
   * @param panel Panel containing the shortcuts.
   * @param constraints Grid bag constraints.
   * @param name Descriptive name of the shortcut.
   * @param shortcut Shortcut.
   */
  private void addLine(
      JPanel panel,
      GridBagConstraints constraints,
      String name,
      ConfigurationValueShortcut shortcut) {

    // Enabled check box
    JCheckBox chk = Utilities.createJCheckBox("", true);
    chkEnabled.add(chk);
    constraints.gridx = columnEnabled;
    constraints.weightx = 0;
    panel.add(chk, constraints);

    // Name
    if (name != null) {
      JLabel label = Utilities.createJLabel(name);
      constraints.gridx = columnName;
      constraints.weightx = 1;
      panel.add(label, constraints);
    }

    // CTRL key
    JToggleButton toggle = Utilities.createJToggleButton("CTRL");
    chkCtrl.add(toggle);
    constraints.gridx = columnCtrl;
    constraints.weightx = 0;
    panel.add(toggle, constraints);

    // ALT key
    toggle = Utilities.createJToggleButton("ALT");
    chkAlt.add(toggle);
    constraints.gridx = columnAlt;
    constraints.weightx = 0;
    panel.add(toggle, constraints);

    // SHIFT key
    toggle = Utilities.createJToggleButton("SHIFT");
    chkShift.add(toggle);
    constraints.gridx = columnShift;
    constraints.weightx = 0;
    panel.add(toggle, constraints);

    // Key
    KeyCodeButton button = new KeyCodeButton(0);
    btnKey.add(button);
    constraints.gridx = columnKey;
    constraints.weightx = 0;
    panel.add(button, constraints);

    // Next line
    constraints.gridy++;
    int line = shortcuts.size();
    shortcuts.add(shortcut);
    setShortcut(line, shortcut);
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
    setShift(line, properties.getShiftKey());
    setKey(line, properties.getKey());
  }

  /**
   * @param line Line number.
   * @param name Shortcut name.
   * @return Shortcut.
   */
  private ConfigurationValueShortcut.ShortcutProperties getShortcut(
      int line, String name) {
    if ((line < 0) || (line >= shortcuts.size())) {
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
    boolean shift = false;
    if (chkShift.get(line) != null) {
      shift = chkShift.get(line).isSelected();
    }
    int key = 0;
    if (btnKey.get(line) != null) {
      key = btnKey.get(line).getKeyCode();
    }
    return new ConfigurationValueShortcut.ShortcutProperties(
        enabled, name, ctrl, alt, shift, key);
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
   * Set the checkbox for the shift column.
   * 
   * @param line Line number.
   * @param value Checkbox value.
   */
  private void setShift(int line, boolean value) {
    setToggleButton(chkShift, line, value);
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
    @Override
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
    @Override
    public boolean dispatchKeyEvent(KeyEvent e) {
      int key = e.getKeyCode();
      if ((key == KeyEvent.VK_ALT) ||
          (key == KeyEvent.VK_ALT_GRAPH) ||
          (key == KeyEvent.VK_BACK_SPACE) ||
          (key == KeyEvent.VK_BEGIN) ||
          (key == KeyEvent.VK_CANCEL) ||
          (key == KeyEvent.VK_CAPS_LOCK) ||
          (key == KeyEvent.VK_CONTROL) ||
          (key == KeyEvent.VK_DELETE) ||
          (key == KeyEvent.VK_DOWN) ||
          (key == KeyEvent.VK_END) ||
          (key == KeyEvent.VK_ENTER) ||
          (key == KeyEvent.VK_LEFT) ||
          (key == KeyEvent.VK_PAGE_DOWN) ||
          (key == KeyEvent.VK_PAGE_UP) ||
          (key == KeyEvent.VK_RIGHT) ||
          (key == KeyEvent.VK_SHIFT) ||
          (key == KeyEvent.VK_TAB) ||
          (key == KeyEvent.VK_UP) ||
          (key == KeyEvent.VK_SHIFT)) {
        return false;
      }
      if (key != KeyEvent.VK_ESCAPE) {
        setKeyCode(key);
      }
      showNotice(false);
      KeyboardFocusManager.getCurrentKeyboardFocusManager().removeKeyEventDispatcher(this);
      return false;
    }
  }
}
