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

package org.wikipediacleaner.gui.swing.basic;

import java.awt.Component;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Arrays;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.StringChecker;


/**
 * Utilities for the Swing GUI. 
 */
public class Utilities {

  private static Log log = LogFactory.getLog(Utilities.class);

  public final static int YES_ALL_OPTION = 101;

  public final static int NO_ALL_OPTION = 102;

  /* ========================================================================== */
  /* Display message box                                                        */
  /* ========================================================================== */

  /**
   * Display an error message.
   * 
   * @param parent Parent component.
   * @param exception Exception.
   */
  public static void displayError(
      Component parent,
      Throwable exception) {
    if ((exception != null) && !(exception instanceof APIException)) {
      exception.printStackTrace();
    }
    if (exception != null) {
      if (exception instanceof APIException) {
        displayMessage(
            parent, GT._("An error occured: {0}", exception.getMessage()),
            null, JOptionPane.ERROR_MESSAGE);
      } else {
        displayMessage(
            parent,
            GT._("An error occured: {0}", exception.getMessage() + " - " + exception.getClass().getName()),
            null, JOptionPane.ERROR_MESSAGE);
      }
    } else {
      displayMessage(
          parent, GT._("An unknown error occured"),
          null, JOptionPane.ERROR_MESSAGE);
    }
  }

  /**
   * Display an error message.
   * 
   * @param parent Parent component.
   * @param message Message.
   */
  public static void displayError(
      Component parent,
      String message) {
    displayMessage(parent, message, null, JOptionPane.ERROR_MESSAGE);
  }

  /**
   * Display an error message.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @param focus Component to give focus to.
   */
  public static void displayError(Component parent, String message, Component focus) {
    displayMessage(parent, message, focus, JOptionPane.ERROR_MESSAGE);
  }

  /**
   * Display a warning message.
   * 
   * @param parent Parent component.
   * @param message Message.
   */
  public static void displayWarning(
      Component parent,
      String message) {
    displayMessage(parent, message, null, JOptionPane.WARNING_MESSAGE);
  }

  /**
   * Display a warning message.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @param focus Component to give focus to.
   */
  public static void displayWarning(Component parent, String message, Component focus) {
    displayMessage(parent, message, focus, JOptionPane.WARNING_MESSAGE);
  }

  /**
   * Display a warning message about a missing parameter in configuration.
   * 
   * @param parent Parent component.
   * @param parameterName Missing parameter.
   */
  public static void displayWarningForMissingConfiguration(Component parent, String parameterName) {
    displayWarning(
        parent,
        GT._("This function requires some configuration.") + "\n" +
        GT._("You need to define the ''{0}'' property in WPCleaner configuration.", parameterName));
  }

  /**
   * Display a warning message about a missing parameters in configuration.
   * 
   * @param parent Parent component.
   * @param parametersName Missing parameters.
   */
  public static void displayWarningForMissingConfiguration(Component parent, List<String> parametersName) {
    if ((parametersName == null) || (parametersName.isEmpty())) {
      return;
    }
    if (parametersName.size() == 1) {
      displayWarning(parent, parametersName.get(0));
      return;
    }
    StringBuilder sb = new StringBuilder();
    for (String parameterName : parametersName) {
      if (sb.length() > 0) {
        sb.append(", ");
      }
      sb.append("'");
      sb.append(parameterName);
      sb.append("'");
    }
    displayWarning(
        parent,
        GT._("This function requires some configuration.") + "\n" +
        GT._("You need to define the {0} properties in WPCleaner configuration.", sb.toString()));
  }

  /**
   * Display a message.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @param focus Component to give focus to.
   * @param messageType Message type.
   */
  private static void displayMessage(
      Component parent,
      String message,
      Component focus,
      int messageType) {
    JOptionPane.showMessageDialog(parent, message, Version.PROGRAM, messageType);
    if (focus != null) {
      focus.requestFocusInWindow();
    }
  }

  /**
   * Display an information message.
   * 
   * @param parent Parent component.
   * @param message Message.
   */
  public static void displayInformationMessage(Component parent, String message) {
    displayMessage(parent, message, null, JOptionPane.INFORMATION_MESSAGE);
  }

  /**
   * Display a message to request a value.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @param value Default value.
   * @param checker String checker to verify the value.
   * @return Value provided by the user.
   */
  public static String askForValue(
      Component parent, String message, String value,
      StringChecker checker) {
    String defaultValue = value;
    while (true) {
      Object result = JOptionPane.showInputDialog(
          parent, message, Version.PROGRAM,
          JOptionPane.QUESTION_MESSAGE, null, null,
          defaultValue);
      if (result == null) {
        return null;
      }
      defaultValue = result.toString();
      if (checker == null) {
        return defaultValue;
      }
      StringChecker.Result ok = checker.checkString(defaultValue);
      if (ok == null) {
        return defaultValue;
      }
      if (ok.isOk()) {
        return ok.getText();
      }
      defaultValue = ok.getText();
      String errorMessage = ok.getMessage();
      if (message != null) {
        displayError(parent, errorMessage);
      }
    }
  }

  /**
   * Display a message to request a value.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @param possibleValues Possible values.
   * @param onlyList Restrict selection to the possible values.
   * @param value Default value.
   * @param checker String checker to verify the value.
   * @return Value provided by the user.
   */
  public static String askForValue(
      Component parent, String message,
      Object[] possibleValues, boolean onlyList, String value,
      StringChecker checker) {
    if ((possibleValues == null) || (possibleValues.length == 0)) {
      return askForValue(parent, message, value, checker);
    }
    String defaultValue = value;
    String other = GT._("Other...");
    Object[] possibles = possibleValues;
    if (!onlyList) {
      possibles = Arrays.copyOf(possibleValues, possibleValues.length + 1);
      possibles[possibles.length - 1] = other;
    }
    while (true) {
      Object result = JOptionPane.showInputDialog(
          parent, message, Version.PROGRAM,
          JOptionPane.QUESTION_MESSAGE, null,
          possibles, defaultValue);
      if (result == null) {
        return null;
      }
      if (result == other) {
        return askForValue(parent, message, defaultValue, checker);
      }
      defaultValue = result.toString();
      if (checker == null) {
        return defaultValue;
      }
      StringChecker.Result ok = checker.checkString(defaultValue);
      if (ok == null) {
        return defaultValue;
      }
      if (ok.isOk()) {
        return ok.getText();
      }
      defaultValue = ok.getText();
      String errorMessage = ok.getMessage();
      if (message != null) {
        displayError(parent, errorMessage);
      }
    }
  }

  /**
   * Display a message to request a value.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @param possibleValues Possible values.
   * @param value Default value.
   * @return Value provided by the user.
   */
  public static Object askForValue(
      Component parent, String message,
      Object[] possibleValues, Object value) {
    if ((possibleValues == null) || (possibleValues.length == 0)) {
      return null;
    }
    return JOptionPane.showInputDialog(
        parent, message, Version.PROGRAM,
        JOptionPane.QUESTION_MESSAGE, null,
        possibleValues, value);
  }

  /**
   * Display a question with Yes/No answers.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @return Answer {@link JOptionPane#YES_OPTION} or {@link JOptionPane#NO_OPTION}.
   */
  public static int displayYesNoWarning(Component parent, String message) {
    return JOptionPane.showConfirmDialog(
        parent, message, Version.PROGRAM,
        JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
  }

  /**
   * Display a question with Yes/No/Cancel answers.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @return Answer {@link JOptionPane#YES_OPTION}, {@link JOptionPane#NO_OPTION} or {@link JOptionPane#CANCEL_OPTION}.
   */
  public static int displayYesNoCancelWarning(Component parent, String message) {
    return JOptionPane.showConfirmDialog(
        parent, message, Version.PROGRAM,
        JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
  }

  /**
   * Display a question with Yes/Yes all/No/No all answers.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @return Answer {@link JOptionPane#YES_OPTION} or {@link JOptionPane#NO_OPTION}.
   */
  public static int displayYesNoAllWarning(Component parent, String message) {
    Object[] options = new Object[] {
        GT._("Yes"),
        GT._("Yes to all"),
        GT._("No"),
        GT._("No to all"),
    }; 
    JOptionPane pane = new JOptionPane(
        message, JOptionPane.WARNING_MESSAGE,
        JOptionPane.YES_NO_OPTION, null, options);
    JDialog dialog = pane.createDialog(parent, Version.PROGRAM);
    dialog.setVisible(true);
    Object selectedValue = pane.getValue();
    if (selectedValue == null) {
      return JOptionPane.CLOSED_OPTION;
    }
    if (options[0].equals(selectedValue)) {
      return JOptionPane.YES_OPTION;
    }
    if (options[1].equals(selectedValue)) {
      return YES_ALL_OPTION;
    }
    if (options[2].equals(selectedValue)) {
      return JOptionPane.NO_OPTION;
    }
    if (options[3].equals(selectedValue)) {
      return NO_ALL_OPTION;
    }
    return JOptionPane.CLOSED_OPTION;
  }

  public static int displayQuestion(Component parent, String message, Object[] values) {
    return JOptionPane.showOptionDialog(
        parent, message, Version.PROGRAM,
        JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE,
        null, values, null);
  }

  /* ========================================================================== */
  /* Component management                                                       */
  /* ========================================================================== */

  /**
   * Create a JButton.
   * 
   * @param message Label text with optional mnemonic inside.
   * @return Button initialized with text and mnemonic.
   */
  public static JButton createJButton(String message) {
    JButton button = new JButton(getLabelWithoutMnemonic(message));
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      button.setMnemonic(mnemonic);
    }
    return button;
  }

  /**
   * Create a JToggleButton.
   * 
   * @param message Label text with optional mnemonic inside.
   * @return Button initialized with text and mnemonic.
   */
  public static JToggleButton createJToggleButton(String message) {
    JToggleButton button = new JToggleButton(getLabelWithoutMnemonic(message));
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      button.setMnemonic(mnemonic);
    }
    return button;
  }

  /**
   * Retrieve a icon.
   * 
   * @param iconName Icon name.
   * @param size Icon size.
   * @return Icon.
   */
  public static ImageIcon getImageIcon(String iconName, EnumImageSize size) {
    ImageIcon icon = null;
    if ((iconName != null) && (size != null)) {
      URL url = Utilities.class.getClassLoader().getResource(
          "org/wikipediacleaner/images/" + size.getFolder() + "/" + iconName);
      if (url != null) {
        icon = new ImageIcon(url);
      }
    }
    return icon;
  }

  /**
   * Create a JButton.
   * 
   * @param iconName Icon name.
   * @param size Icon size.
   * @param message Label text with optional mnemonic inside.
   * @param showMessage Use message for the button text or for tooltip.
   * @return Button initialized with text and mnemonic.
   */
  public static JButton createJButton(
      String iconName, EnumImageSize size,
      String message, boolean showMessage) {
    ImageIcon icon = getImageIcon(iconName, size);
    JButton button = null;
    if (icon != null) {
      if (showMessage) {
        button = new JButton(getLabelWithoutMnemonic(message), icon);
      } else {
        button = new JButton(icon);
        button.setToolTipText(getLabelWithoutMnemonic(message));
      }
    } else {
      button = new JButton(getLabelWithoutMnemonic(message));
    }
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      button.setMnemonic(mnemonic);
    }
    return button;
  }

  /**
   * Create a JToggleButton.
   * 
   * @param iconName Icon name.
   * @param size Icon size.
   * @param message Label text with optional mnemonic inside.
   * @param showMessage Use message for the button text or for tooltip.
   * @return Button initialized with text and mnemonic.
   */
  public static JToggleButton createJToggleButton(
      String iconName, EnumImageSize size,
      String message, boolean showMessage) {
    ImageIcon icon = getImageIcon(iconName, size);
    JToggleButton button = null;
    if (icon != null) {
      if (showMessage) {
        button = new JToggleButton(getLabelWithoutMnemonic(message), icon);
      } else {
        button = new JToggleButton(icon);
        button.setToolTipText(getLabelWithoutMnemonic(message));
      }
    } else {
      button = new JToggleButton(getLabelWithoutMnemonic(message));
    }
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      button.setMnemonic(mnemonic);
    }
    return button;
  }

  /**
   * Create a JCheckBox.
   * 
   * @param message Label text with optional mnemonic inside.
   * @param selected Flag indicating if the check box is selected.
   * @return Check Box initialized with text and mnemonic.
   */
  public static JCheckBox createJCheckBox(String message, boolean selected) {
    JCheckBox checkbox = new JCheckBox(getLabelWithoutMnemonic(message), selected);
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      checkbox.setMnemonic(mnemonic);
    }
    return checkbox;
  }

  /**
   * Create a JRadioButton.
   * 
   * @param message Label text with optional mnemonic inside.
   * @param selected Flag indicating if the check box is selected.
   * @return Check Box initialized with text and mnemonic.
   */
  public static JRadioButton createJRadioButton(String message, boolean selected) {
    JRadioButton button = new JRadioButton(getLabelWithoutMnemonic(message), selected);
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      button.setMnemonic(mnemonic);
    }
    return button;
  }

  /**
   * Create a JLabel.
   * 
   * @param message Label text with optional mnemonic inside.
   * @return Label initialized with text and mnemonic.
   */
  public static JLabel createJLabel(String message) {
    JLabel label = new JLabel(getLabelWithoutMnemonic(message));
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      label.setDisplayedMnemonic(mnemonic);
    }
    return label;
  }

  /**
   * Create a JTextField.
   * 
   * @param value Default value.
   * @param columns Number of columns.
   * @return JTextField.
   */
  public static JTextField createJTextField(String value, int columns) {
    JTextField text = new JTextField(value, columns);
    text.addKeyListener(new CopyPasteListener(text));
    return text;
  }

  /**
   * Copy/Paste listener.
   */
  private static class CopyPasteListener implements KeyListener {

    /**
     * Text field.
     */
    private final JTextComponent text;

    /**
     * @param text Text component.
     */
    public CopyPasteListener(JTextComponent text) {
      this.text = text;
    }

    /**
     * Listener for KeyTyped events.
     * 
     * @param e Event.
     * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
     */
    public void keyTyped(KeyEvent e) {
      //
    }

    /**
     * Listener for KeyPressed events.
     * 
     * @param e Event.
     * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
     */
    public void keyPressed(KeyEvent e) {
      if ((e.getKeyCode() == KeyEvent.VK_V) &&
          ((e.getModifiersEx() & InputEvent.CTRL_DOWN_MASK) != 0)) {
        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        Transferable clipData = clipboard.getContents(clipboard);
        try {
          String s = (String) clipData.getTransferData(DataFlavor.stringFlavor);
          int start = text.getSelectionStart();
          int end = text.getSelectionEnd();
          if (end > start) {
            text.getDocument().remove(start, end - start);
          }
          text.getDocument().insertString(start, s, null);
          e.consume();
        } catch (Exception ex) {
          //
        }
      }
    }

    /**
     * Listener for KeyRelease events.
     * 
     * @param e Event.
     * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
     */
    public void keyReleased(KeyEvent e) {
      //
    }
    
  }

  /**
   * Create a JMenu.
   * 
   * @param message Label text with optional mnemonic inside.
   * @return Menu initialized with text and mnemonic.
   */
  public static JMenu createJMenu(String message) {
    JMenu menu = new JMenu(getLabelWithoutMnemonic(message));
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      menu.setMnemonic(mnemonic);
    }
    return menu;
  }

  /**
  * Create a JMenuItem.
  * 
  * @param message Label text with optional mnemonic inside.
  * @return Menu item initialized with text and mnemonic.
  */
  public static JMenuItem createJMenuItem(String message) {
    JMenuItem menuItem = new JMenuItem(getLabelWithoutMnemonic(message));
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      menuItem.setMnemonic(mnemonic);
    }
    return menuItem;
  }

  /**
   * Create a JCheckBoxMenuItem.
   * 
   * @param message Label text with optional mnemonic inside.
   * @param checked Flag indicating if menu item should be checked.
   * @return Menu item initialized with text and mnemonic.
   */
  public static JCheckBoxMenuItem createJCheckBoxMenuItm(String message, boolean checked) {
    JCheckBoxMenuItem menuItem = new JCheckBoxMenuItem(
        getLabelWithoutMnemonic(message), checked);
    char mnemonic = getMnemonic(message);
    if (mnemonic != ' ') {
      menuItem.setMnemonic(mnemonic);
    }
    return menuItem;
  }

  /**
   * Return the label to display.
   * 
   * @param label Label with optional mnemonic (&).
   * @return Label without optional mnemonic.
   */
  private static String getLabelWithoutMnemonic(String label) {
    if (label == null) {
      return null;
    }
    int index = label.indexOf('&');
    if (index == -1) {
      return label;
    }
    return label.substring(0, index) +
      ((index < label.length() - 1) ? label.substring(index + 1) : "");
  }

  /**
   * Return the mnemonic of the label.
   * 
   * @param label Label with optional mnemonic (&).
   * @return Mnemonic.
   */
  private static char getMnemonic(String label) {
    if (label == null) {
      return ' ';
    }
    int index = label.indexOf('&');
    if ((index == -1) || (index == label.length() - 1)){
      return ' ';
    }
    return label.charAt(index + 1);
  }

  /* ========================================================================== */
  /* Desktop management                                                         */
  /* ========================================================================== */

  /**
   * @return Flag indicating if desktop is supported.
   */
  public static boolean isDesktopSupported() {
    try {
      Class<?> desktop = Class.forName("java.awt.Desktop");
      Method method = desktop.getMethod("isDesktopSupported", (Class[]) null);
      return (Boolean) method.invoke(null, (Object[]) null);
    } catch (ClassNotFoundException e) {
      log.debug("ClassNotFoundException: " + e.getMessage());
      // Nothing to be done, JVM < 6
    } catch (NoSuchMethodException e) {
      log.error("NoSuchMethodException: " + e.getMessage());
    } catch (InvocationTargetException e) {
      log.error("InvocationTargetException: " + e.getMessage());
    } catch (IllegalAccessException e) {
      log.error("IllegalAccessException: " + e.getMessage());
    } catch (ClassCastException e) {
      log.error("ClassCastException: " + e.getMessage());
    } catch (Throwable e) {
      log.error("Throwable: " + e.getClass().getName() + " - " + e.getMessage());
    }
    return false;
  }

  /**
   * Display an URI in the default browser.
   * 
   * @param uri URI.
   */
  public static void browseURL(URI uri) {
    if (isDesktopSupported()) {
      try {
        Class<?> desktopClass = Class.forName("java.awt.Desktop");
        Method method = desktopClass.getMethod("getDesktop", (Class[]) null);
        Object desktop = method.invoke(null, (Object[]) null);
        method = desktopClass.getMethod("browse", new Class[] { URI.class });
        method.invoke(desktop, new Object[] { uri });
      } catch (ClassNotFoundException e) {
        log.debug("ClassNotFoundException: " + e.getMessage());
        // Nothing to be done, JVM < 6
      } catch (NoSuchMethodException e) {
        log.error("NoSuchMethodException: " + e.getMessage());
      } catch (InvocationTargetException e) {
        log.error("InvocationTargetException: " + e.getMessage());
      } catch (IllegalAccessException e) {
        log.error("IllegalAccessException: " + e.getMessage());
      } catch (ClassCastException e) {
        log.error("ClassCastException: " + e.getMessage());
      } catch (Throwable e) {
        log.error("Throwable: " + e.getClass().getName() + " - " + e.getMessage());
      }
    }
  }

  /**
   * Display an URL in the default browser.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param action Page action.
   */
  public static void browseURL(EnumWikipedia wiki, String title, String action) {
    Configuration config = Configuration.getConfiguration();
    boolean secured = config.getBoolean(null, ConfigurationValueBoolean.SECURE_URL);
    browseURL(wiki.getSettings().getURL(title, action, secured));
  }

  /**
   * Display an URL in the default browser.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param redirect Flag indicating if redirects should be followed.
   */
  public static void browseURL(EnumWikipedia wiki, String title, boolean redirect) {
    Configuration config = Configuration.getConfiguration();
    boolean secured = config.getBoolean(null, ConfigurationValueBoolean.SECURE_URL);
    browseURL(wiki.getSettings().getURL(title, redirect, secured));
  }

  /**
   * Display an URL in the default browser.
   * 
   * @param url URL.
   */
  public static void browseURL(String url) {
    try {
      browseURL(new URI(url));
    } catch (URISyntaxException e) {
      // Nothing to be done
      log.error("Error viewing page: " + e.getMessage());
    }
  }

  /* ========================================================================== */
  /* Various utilities                                                          */
  /* ========================================================================== */

  /**
   * Create a RowSorter for a JTable (work only for JDK 6).
   * @see JTable#setRowSorter(javax.swing.RowSorter)
   * 
   * @param table Table.
   * @param model Table model.
   */
  public static void addRowSorter(JTable table, TableModel model) {
    try {
      Class<?> tableRowSorterClass = Class.forName("javax.swing.table.TableRowSorter");
      Constructor ctor = tableRowSorterClass.getConstructor(new Class[] { TableModel.class });
      Object rowSorter = ctor.newInstance(model);
      Class rowSorterClass = Class.forName("javax.swing.RowSorter");
      Method method = table.getClass().getMethod("setRowSorter", new Class[] { rowSorterClass });
      method.invoke(table, new Object[] { rowSorter });
    } catch (ClassNotFoundException e) {
      log.debug("ClassNotFoundException: " + e.getMessage());
      // Nothing to be done, JVM < 6
    } catch (NoSuchMethodException e) {
      log.error("NoSuchMethodException: " + e.getMessage());
    } catch (InvocationTargetException e) {
      log.error("InvocationTargetException: " + e.getMessage());
    } catch (IllegalAccessException e) {
      log.error("IllegalAccessException: " + e.getMessage());
    } catch (ClassCastException e) {
      log.error("ClassCastException: " + e.getMessage());
    } catch (Throwable e) {
      log.error("Throwable: " + e.getClass().getName() + " - " + e.getMessage());
    }
  }

  /**
   * Convert a table row index to model index (work only for JDK 6).
   * @see JTable#convertRowIndexToModel(int)
   * 
   * @param table Table.
   * @param row Row.
   * @return Converted row.
   */
  public static int convertRowIndexToModel(JTable table, int row) {
    try {
      Method method = table.getClass().getMethod("convertRowIndexToModel", new Class[] { int.class });
      Object result = method.invoke(table, new Object[] { row });
      row = ((Integer) result).intValue();
    } catch (NoSuchMethodException e) {
      log.debug("NoSuchMethodException: " + e.getMessage());
      // Nothing to be done, JVM < 6
    } catch (InvocationTargetException e) {
      log.error("InvocationTargetException: " + e.getMessage());
    } catch (IllegalAccessException e) {
      log.error("IllegalAccessException: " + e.getMessage());
    } catch (ClassCastException e) {
      log.error("ClassCastException: " + e.getMessage());
    } catch (Throwable e) {
      log.error("Throwable: " + e.getClass().getName() + " - " + e.getMessage());
    }
    return row;
  }
}
