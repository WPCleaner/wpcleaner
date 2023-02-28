/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.font.TextAttribute;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractButton;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.action.FindTextAction;
import org.wikipediacleaner.gui.swing.action.ReplaceLinkAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * A text component to color / edit MediaWiki text.
 */
public class MWPane
    extends JTextPane {

  private static final long serialVersionUID = 3225120886653438117L;

  public static final String PROPERTY_MODIFIED = "ModifiedProperty";

  private final EnumWikipedia wikipedia;
  private Page page;
  private final BasicWindow window;

  private MWPaneFormatter formatter;

  private static final KeyStroke lastLinkKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_L, InputEvent.CTRL_MASK);
  private static final KeyStroke lastReplaceKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_R, InputEvent.CTRL_MASK);

  private boolean isModified = false;
  boolean isEditable = true;
  boolean isInInternalModification = false;

  private transient MWPanePopupListener popupListener;

  public static KeyStroke getLastLinkKeyStroke() {
    return lastLinkKeyStroke;
  }

  public static KeyStroke getLastReplaceKeyStroke() {
    return lastReplaceKeyStroke;
  }

  /**
   * Construct a MediaWikiPane.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param window Window containing the pane.
   */
  public MWPane(EnumWikipedia wikipedia, Page page, BasicWindow window) {
    super();
    this.wikipedia = wikipedia;
    this.page = page;
    this.window = window;
    this.selectionManager = new MWPaneSelectionManager(this);
    this.undoManager = new MWPaneUndoManager(this);
    this.formatter = new MWPaneBasicFormatter();
    initialize();
  }

  /**
   * @param page Page.
   */
  public void setWikiPage(Page page) {
    this.page = page;
  }

  /**
   * @return page Page.
   */
  public Page getWikiPage() {
    return page;
  }

  /**
   * @return Wikipedia
   */
  public EnumWikipedia getWikipedia() {
    return wikipedia;
  }

  /**
   * @return Flag indicating if the document has been modified.
   */
  public boolean isModified() {
    return isModified;
  }

  /**
   * @param modified New status of the document
   */
  public void setModified(boolean modified) {
    if (isModified != modified) {
      boolean oldValue = isModified;
      isModified = modified;
      undoManager.updateUndoButtons();
      firePropertyChange(PROPERTY_MODIFIED, oldValue, isModified);
    }
  }

  /**
   * Initialize styles. 
   */
  private void initialize() {
    boolean oldState = isInInternalModification;
    isInInternalModification = true;
    this.setComponentOrientation(wikipedia.getSettings().getComponentOrientation());
    StyledDocument doc = MWPaneFormatter.createDocument();
    setStyledDocument(doc);
    doc.addDocumentListener(new DocumentListener() {

      /* (non-Javadoc)
       * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event.DocumentEvent)
       */
      @Override
      public void changedUpdate(@SuppressWarnings("unused") DocumentEvent e) {
        changeDocument();
      }

      /* (non-Javadoc)
       * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event.DocumentEvent)
       */
      @Override
      public void insertUpdate(@SuppressWarnings("unused") DocumentEvent e) {
        changeDocument();
      }

      /* (non-Javadoc)
       * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.DocumentEvent)
       */
      @Override
      public void removeUpdate(@SuppressWarnings("unused") DocumentEvent e) {
        changeDocument();
      }

      public void changeDocument() {
        if (!isModified() && !isInInternalModification) {
          setModified(true);
        }
      }
    });

    ActionMap actionMap = getActionMap();
    InputMap inputMapFocused = getInputMap();
    InputMap inputMapInFocused = getInputMap(WHEN_IN_FOCUSED_WINDOW);
    KeyStroke keyStroke = null;

    setPopupListener(new MWPaneBasicPopupListener(wikipedia, window));

    keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F, InputEvent.CTRL_MASK);
    inputMapFocused.put(keyStroke, "find-text");
    actionMap.put("find-text", new FindTextAction());

    inputMapInFocused.put(lastLinkKeyStroke, "last-link");
    actionMap.put("last-link", new ReplaceLinkAction(false));

    inputMapInFocused.put(lastReplaceKeyStroke, "last-replace");
    actionMap.put("last-replace", new ReplaceLinkAction(true));

    isInInternalModification = oldState;
    setModified(false);

    // change pane default font family/size
    Configuration config = Configuration.getConfiguration();
    Map<TextAttribute, Object> attributes = new HashMap<>();
    attributes.put(TextAttribute.FAMILY, config.getString(null, ConfigurationValueString.FONT_NAME_EDITOR));
    attributes.put(TextAttribute.SIZE, config.getInt(null, ConfigurationValueInteger.FONT_SIZE_EDITOR));
    this.setFont(this.getFont().deriveFont(attributes));
  }

  /**
   * @param listener Popup listener.
   */
  public void setPopupListener(MWPanePopupListener listener) {

    // Remove old listener
    if (popupListener != null) {
      removeKeyListener(popupListener);
      removeMouseListener(popupListener);
    }

    // Add new listener
    popupListener = listener;
    if (popupListener != null) {
      addKeyListener(popupListener);
      addMouseListener(popupListener);
    }
  }

  /* (non-Javadoc)
   * @see javax.swing.JEditorPane#setText(java.lang.String)
   */
  @Override
  public void setText(String t) {
    setTextInternal(t, true, true);
  }

  /**
   * Change text.
   * 
   * @param t New text.
   */
  public void changeText(String t) {
    if (t == null) {
      return;
    }
    String oldText = getText();
    if (!oldText.equals(t)) {
      setTextInternal(t, false, false);
      setModified(true);
    }
  }

  /**
   * Enabling changing text without resetting the modified flag.
   * 
   * @param t New text.
   * @param resetModified Flag indicating if the modified flag should be reseted.
   * @param validate Flag indicating if the text should be validated.
   */
  private void setTextInternal(
      String t,
      boolean resetModified,
      boolean validate) {
    boolean oldState = isInInternalModification;
    isInInternalModification = true;
    super.setText(t);
    setCaretPosition(0);
    moveCaretPosition(0);
    resetAttributes();
    isInInternalModification = oldState;
    if (resetModified) {
      setModified(false);
      undoManager.clear();
    }
    if (validate) {
      undoManager.validateCurrentText();
    }
  }

  @Override
  public void setEditable(boolean editable) {
    setEditableInternal(editable);
    this.isEditable = editable; 
  }

  /**
   * Enabling to render this component editable or not temporarily.
   * 
   * @param editable
   */
  void setEditableInternal(boolean editable) {
    super.setEditable(editable);
  }

  /* ========================================================================= */
  /* Selection management                                                      */
  /* ========================================================================= */

  /**
   * Selection manager.
   */
  private final MWPaneSelectionManager selectionManager;

  /**
   * @return Selection manager.
   */
  public MWPaneSelectionManager getSelectionManager() {
    return selectionManager;
  }

  /* ========================================================================= */
  /* Formatting management                                                     */
  /* ========================================================================= */

  /**
   * @param formatter Formatter.
   */
  public void setFormatter(MWPaneFormatter formatter) {
    this.formatter = formatter;
    resetAttributes();
  }

  /**
   * @return Formatter.
   */
  public MWPaneFormatter getFormatter() {
    return formatter;
  }

  /**
   * Reset attributes of the document.
   * This method should be called after modifications are done.
   */
  public void resetAttributes() {

    // Check formatter
    if (formatter == null) {
      return;
    }

    boolean oldState = isInInternalModification;
    isInInternalModification = true;

    // First remove MediaWiki styles
    String contents = getText();
    PageAnalysis pageAnalysis = (page != null) ? page.getAnalysis(contents, true) : null;
    formatter.format(this, pageAnalysis);

    isInInternalModification = oldState;

    if (!isInInternalModification) {
      undoManager.validateCurrentText();
    }
  }

  /* ========================================================================= */
  /* Font management                                                           */
  /* ========================================================================= */

  /**
   * @return Flag indicating if all the text can be displayed.
   */
  public boolean canDisplayAllText() {
    String text = getText();
    Font font = getFont();
    if ((text != null) && (font != null)) {
      return (font.canDisplayUpTo(text) == -1);
    }
    return true;
  }

  /**
   * @return List of fonts that can display all characters.
   */
  public List<Font> getPossibleFonts() {
    String text = getText();
    List<Font> possibleFonts = new ArrayList<>();
    Font[] allFonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAllFonts();
    for (int i = 0; i < allFonts.length; i++) {
      if (allFonts[i].canDisplayUpTo(text) == -1) {
        possibleFonts.add(allFonts[i]);
      }
    }
    return possibleFonts;
  }

  /* ========================================================================= */
  /* Check Box management                                                      */
  /* ========================================================================= */

  private AbstractButton chkAddNote;
  private AbstractButton chkCreateDabWarning;
  private AbstractButton chkUpdateDabWarning;

  /**
   * @return Button used for adding a note in the Talk page.
   */
  public AbstractButton getCheckBoxAddNote() {
    return chkAddNote;
  }

  /**
   * @param chk Button used for adding a note in the Talk page.
   */
  public void setCheckBoxAddNote(AbstractButton chk) {
    chkAddNote = chk;
  }

  /**
   * @return Button used for creating disambiguation warning in the Talk page.
   */
  public AbstractButton getCheckBoxCreateDabWarning() {
    return chkCreateDabWarning;
  }

  /**
   * @param chk Button used for creating disambiguation warning in the Talk page.
   */
  public void setCheckBoxCreateDabWarning(AbstractButton chk) {
    chkCreateDabWarning = chk;
  }

  /**
   * @return Button used for updating disambiguation warning in the Talk page.
   */
  public AbstractButton getCheckBoxUpdateDabWarning() {
    return chkUpdateDabWarning;
  }

  /**
   * @param chk Button used for updating disambiguation warning in the Talk page.
   */
  public void setCheckBoxUpdateDabWarning(AbstractButton chk) {
    chkUpdateDabWarning = chk;
  }

  /* ========================================================================= */
  /* Undo / Redo management                                                    */
  /* ========================================================================= */

  /**
   * Undo / Redo management.
   */
  private final MWPaneUndoManager undoManager;

  /**
   * @return Undo / Redo management.
   */
  public MWPaneUndoManager getUndoManager() {
    return undoManager;
  }

  /* ========================================================================= */
  /* Complex Pane management                                                   */
  /* ========================================================================= */

  private MWPaneTitleTreeManager treeManager;

  /**
   * Construct a complex MediaWikiPane.
   * 
   * @param textPane Existing MediaWikiPane.
   * @return Complex component containing a MediaWikiPane.
   */
  public static JComponent createComplexPane(
      final MWPane textPane) {
    if (textPane == null) {
      return null;
    }
    if (textPane.treeManager == null) {
      textPane.treeManager = new MWPaneTitleTreeManager(textPane);
    }
    return textPane.treeManager.getComponent();
  }

  /**
   * Display or hide Table of Contents.
   */
  public void toggleToc() {
    if (treeManager != null) {
      treeManager.toggleToc();
    }
  }

  /**
   * Display Table of Contents.
   * 
   * @param title Title to be selected.
   */
  public void displayToc(PageElementTitle title) {
    if (treeManager != null) {
      treeManager.displayToc(title);
    }
  }

  /**
   * Hide Table of Contents.
   */
  public void hideToc() {
    if (treeManager != null) {
      treeManager.hideToc();
    }
  }
}
