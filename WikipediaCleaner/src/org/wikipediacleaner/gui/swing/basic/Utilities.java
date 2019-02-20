/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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

import javax.swing.AbstractButton;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.action.ActionClick;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.ConfigurationValueShortcut.ShortcutProperties;


/**
 * Utilities for the Swing GUI. 
 */
public class Utilities {

  private static Logger log = LoggerFactory.getLogger(Utilities.class);

  public final static int YES_ALL_OPTION = 101;

  public final static int NO_ALL_OPTION = 102;

  private final static String URL_CONFIGURATION_HELP = "http://en.wikipedia.org/wiki/Wikipedia:WPCleaner/Configuration/Help";

  // ==========================================================================
  // Manage event thread
  // ==========================================================================

  /**
   * Utility method to run a task in the event dispatch thread.
   * 
   * @param doRun Task to be run.
   */
  public static void runInEventDispatchThread(Runnable doRun) {
    if (doRun == null) {
      return;
    }
    if (SwingUtilities.isEventDispatchThread()) {
      doRun.run();
    } else {
      try {
        SwingUtilities.invokeAndWait(doRun);
      } catch (InvocationTargetException | InterruptedException e) {
        log.error("Error waiting for execution on event dispatch thread", e);
      }
    }
  }

  // ==========================================================================
  // Display message box
  // ==========================================================================

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
            parent, GT._T("An error occurred: {0}", exception.getMessage()),
            null, JOptionPane.ERROR_MESSAGE);
      } else {
        displayMessage(
            parent,
            GT._T("An error occurred: {0}", exception.getMessage() + " - " + exception.getClass().getName()),
            null, JOptionPane.ERROR_MESSAGE);
      }
    } else {
      displayMessage(
          parent, GT._T("An unknown error occurred"),
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
   * Display a message about a missing parameter in configuration.
   * 
   * @param parent Parent component.
   * @param parameterName Missing parameter.
   */
  public static void displayMessageForMissingConfiguration(Component parent, String parameterName) {
    displayMissingConfiguration(
        parent,
        GT._T("You need to define the ''{0}'' property in WPCleaner configuration.", parameterName));
  }

  /**
   * Display a message about a missing parameters in configuration.
   * 
   * @param parent Parent component.
   * @param parametersName Missing parameters.
   */
  public static void displayMessageForMissingConfiguration(Component parent, List<String> parametersName) {
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
    displayMissingConfiguration(
        parent,
        GT._T("You need to define the {0} properties in WPCleaner configuration.", sb.toString()));
  }

  /**
   * Display a message about missing elements in configuration.
   * 
   * @param parent Parent component.
   * @param message Message.
   */
  public static void displayMissingConfiguration(Component parent, String message) {
    String fullMessage = GT._T("This function requires some configuration.");
    if ((message != null) && (message.trim().length() > 0)) {
      fullMessage += "\n" + message;
    }
    if (Utilities.isDesktopSupported()) {
      int answer = displayYesNoWarning(
          parent,
          fullMessage + "\n" + GT._T("Do you want to display help on configuring WPCleaner ?"));
      if (answer == JOptionPane.YES_OPTION) {
        Utilities.browseURL(URL_CONFIGURATION_HELP);
      }
    } else {
      displayWarning(parent, message);
    }
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
    runInEventDispatchThread(
        new TaskMessageDialog(parent, message, focus, messageType));
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
   * Display an URL message.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @param url URL.
   */
  public static void displayUrlMessage(Component parent, String message, String url) {
    // TODO: Let users copy / paste the url
    displayInformationMessage(parent, message + "\n" + url);
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
      TaskInputDialog task = new TaskInputDialog(
          parent, message, null, defaultValue);
      runInEventDispatchThread(task);
      Object result = task.getResult();
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
    String other = GT._T("Other...");
    Object[] possibles = possibleValues;
    if (!onlyList) {
      possibles = Arrays.copyOf(possibleValues, possibleValues.length + 1);
      possibles[possibles.length - 1] = other;
    }
    while (true) {
      TaskInputDialog task = new TaskInputDialog(
          parent, message, possibles, defaultValue);
      runInEventDispatchThread(task);
      Object result = task.getResult();
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
    TaskInputDialog task = new TaskInputDialog(
        parent, message, possibleValues, value);
    runInEventDispatchThread(task);
    return task.getResult();
  }

  /**
   * Display a question with Yes/No answers.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @return Answer {@link JOptionPane#YES_OPTION} or {@link JOptionPane#NO_OPTION}.
   */
  public static int displayYesNoWarning(Component parent, String message) {
    TaskConfirmDialog task = new TaskConfirmDialog(
        parent, message, JOptionPane.YES_NO_OPTION);
    runInEventDispatchThread(task);
    return task.getResult();
  }

  /**
   * Display a question with Yes/No/Cancel answers.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @return Answer {@link JOptionPane#YES_OPTION}, {@link JOptionPane#NO_OPTION} or {@link JOptionPane#CANCEL_OPTION}.
   */
  public static int displayYesNoCancelWarning(Component parent, String message) {
    TaskConfirmDialog task = new TaskConfirmDialog(
        parent, message, JOptionPane.YES_NO_CANCEL_OPTION);
    runInEventDispatchThread(task);
    return task.getResult();
  }

  /**
   * Display a question with Yes/Yes all/No/No all answers.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @return Answer {@link JOptionPane#YES_OPTION} or {@link JOptionPane#NO_OPTION} or YES_ALL_OPTION or NO_ALL_OPTION.
   */
  public static int displayYesNoAllWarning(Component parent, String message) {
    TaskYesNoAllDialog task = new TaskYesNoAllDialog(parent, message);
    runInEventDispatchThread(task);
    return task.getResult();
  }

  /**
   * Display a question with possible values.
   * 
   * @param parent Parent component.
   * @param message Message.
   * @param values Possible values.
   * @return Selected value.
   */
  public static int displayQuestion(Component parent, String message, Object[] values) {
    TaskOptionDialog task = new TaskOptionDialog(parent, message, values);
    runInEventDispatchThread(task);
    return task.getResult();
  }

  // ==========================================================================
  // Shortcut management
  // ==========================================================================

  /**
   * Apply a shortcut to a button.
   * 
   * @param button Button.
   * @param shortcut Shortcut.
   * @param message Related message.
   */
  private static void setShortcut(
      AbstractButton button,
      ShortcutProperties shortcut,
      String message) {
    if ((shortcut == null) || (shortcut.useMnemonic())) {
      int mnemonic = getMnemonic(message);
      if ((mnemonic == -1) && (shortcut != null)) {
        mnemonic = shortcut.getKey();
      }
      if (mnemonic != -1) {
        button.setMnemonic(mnemonic);
      }
    } else if (shortcut.getEnabled()) {
      String actionName = "action_" + shortcut.getName();
      InputMap inputMap = button.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
      inputMap.put(KeyStroke.getKeyStroke(shortcut.getKey(), shortcut.getModifiers()), actionName);
      button.getActionMap().put(actionName, new ActionClick(button));
      if (message != null) {
        int index = message.indexOf(shortcut.getKey());
        if ((index >= 0) && (index < button.getText().length())) {
          button.setDisplayedMnemonicIndex(index);
        }
      }
    }
  }

  /**
   * @param shortcut Shortcut definition.
   * @return Shortcut.
   */
  private static ShortcutProperties getShortcut(ConfigurationValueShortcut shortcut) {
    if (shortcut == null) {
      return null;
    }
    Configuration config = Configuration.getConfiguration();
    return config.getShortcut(shortcut);
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
  private static int getMnemonic(String label) {
    if (label == null) {
      return 0;
    }
    int index = label.indexOf('&');
    if ((index == -1) || (index == label.length() - 1)){
      return -1;
    }
    return label.charAt(index + 1);
  }

  // ==========================================================================
  // Button management
  // ==========================================================================

  /**
   * Create a JButton.
   * 
   * @param message Label text with optional mnemonic inside.
   * @param shortcut Shortcut information.
   * @return Button initialized with text and mnemonic.
   */
  public static JButton createJButton(
      String message,
      ConfigurationValueShortcut shortcut) {
    String label = getLabelWithoutMnemonic(message);
    JButton button = new JButton(label);
    ShortcutProperties shortcutP = getShortcut(shortcut);
    if ((shortcutP != null) && (shortcutP.getEnabled())) {
      String fullLabel = label + shortcutP.getDescription();
      button.setToolTipText(fullLabel);
    }
    setShortcut(button, shortcutP, message);
    return button;
  }

  /**
   * Create a JButton.
   * 
   * @param iconName Icon name.
   * @param size Icon size.
   * @param message Label text with optional mnemonic inside.
   * @param showMessage Use message for the button text or for tooltip.
   * @param shortcut Shortcut information.
   * @return Button initialized with text and mnemonic.
   */
  public static JButton createJButton(
      String iconName, EnumImageSize size,
      String message, boolean showMessage,
      ConfigurationValueShortcut shortcut) {
    ShortcutProperties shortcutP = getShortcut(shortcut);
    return createJButton(iconName, size, message, showMessage, shortcutP);
  }

  /**
   * Create a JButton.
   * 
   * @param iconName Icon name.
   * @param size Icon size.
   * @param message Label text with optional mnemonic inside.
   * @param showMessage Use message for the button text or for tooltip.
   * @param shortcut Shortcut information.
   * @return Button initialized with text and mnemonic.
   */
  private static JButton createJButton(
      String iconName, EnumImageSize size,
      String message, boolean showMessage,
      ShortcutProperties shortcut) {
    ImageIcon icon = getImageIcon(iconName, size);
    JButton button = null;
    String label = getLabelWithoutMnemonic(message);
    String fullLabel = (shortcut != null) ? label + shortcut.getDescription() : label;
    if (icon != null) {
      if (showMessage) {
        button = new JButton(label, icon);
      } else {
        button = new JButton(icon);
        label = null;
      }
    } else {
      button = new JButton(label);
    }
    button.setToolTipText(fullLabel);
    setShortcut(button, shortcut, message);
    return button;
  }

  // ==========================================================================
  // Toggle Button management
  // ==========================================================================

  /**
   * Create a JToggleButton.
   * 
   * @param message Label text with optional mnemonic inside.
   * @return Button initialized with text and mnemonic.
   */
  public static JToggleButton createJToggleButton(String message) {
    JToggleButton button = new JToggleButton(getLabelWithoutMnemonic(message));
    int mnemonic = getMnemonic(message);
    if (mnemonic != -1) {
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
    setShortcut(button, null, message);
    return button;
  }

  // ==========================================================================
  // Check box and radio button management
  // ==========================================================================

  /**
   * Create a JCheckBox.
   * 
   * @param message Label text with optional mnemonic inside.
   * @param selected Flag indicating if the check box is selected.
   * @return Check Box initialized with text and mnemonic.
   */
  public static JCheckBox createJCheckBox(String message, boolean selected) {
    JCheckBox checkbox = new JCheckBox(getLabelWithoutMnemonic(message), selected);
    setShortcut(checkbox, null, message);
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
    setShortcut(button, null, message);
    return button;
  }

  // ==========================================================================
  // Label management
  // ==========================================================================

  /**
   * Create a JLabel.
   * 
   * @param message Label text with optional mnemonic inside.
   * @return Label initialized with text and mnemonic.
   */
  public static JLabel createJLabel(String message) {
    JLabel label = new JLabel(getLabelWithoutMnemonic(message));
    int mnemonic = getMnemonic(message);
    if (mnemonic != -1) {
      label.setDisplayedMnemonic(mnemonic);
    }
    return label;
  }

  // ==========================================================================
  // Text management
  // ==========================================================================

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
    @Override
    public void keyTyped(KeyEvent e) {
      //
    }

    /**
     * Listener for KeyPressed events.
     * 
     * @param e Event.
     * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
     */
    @Override
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
    @Override
    public void keyReleased(KeyEvent e) {
      //
    }
    
  }

  // ==========================================================================
  // Icon management
  // ==========================================================================

  /**
   * Retrieve an icon.
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

  // ==========================================================================
  // Menu management
  // ==========================================================================

  /**
   * Create a JMenu.
   * 
   * @param message Label text with optional mnemonic inside.
   * @return Menu initialized with text and mnemonic.
   */
  public static JMenu createJMenu(String message) {
    JMenu menu = new JMenu(getLabelWithoutMnemonic(message));
    setShortcut(menu, null, message);
    return menu;
  }

  /**
  * Create a JMenuItem.
  * 
  * @param message Label text with optional mnemonic inside.
  * @param asIs True if the message should be used as is (no mnemonic).
  * @return Menu item initialized with text and mnemonic.
  */
  public static JMenuItem createJMenuItem(String message, boolean asIs) {
    JMenuItem menuItem = new JMenuItem(asIs ? message : getLabelWithoutMnemonic(message));
    if (!asIs) {
      setShortcut(menuItem, null, message);
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
    setShortcut(menuItem, null, message);
    return menuItem;
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
