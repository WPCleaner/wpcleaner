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

import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextPane;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.StyledDocument;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.PageUtilities;
import org.wikipediacleaner.gui.swing.action.FindTextAction;
import org.wikipediacleaner.gui.swing.action.ReplaceLinkAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * A text component to colorize / edit MediaWiki text.
 */
public class MWPane
    extends JTextPane {

  /* Test for patterns
  public static void main(String[] args) {
    //Pattern pattern = Pattern.compile("\\{\\{(loupe(?:\\|((?:(?:[^\\{\\}]*)|(?:\\{\\{\\!\\}\\}))*))?)\\}\\}");
    Pattern pattern = Pattern.compile("\\{\\{(loupe(?:\\|((?:(?:[^\\{\\}])|(?:\\{\\{\\!\\}\\}))*))?)\\}\\}");
    Matcher matcher = pattern.matcher("{{loupe|c=U [[ABCDEFgénéral]] fut {{rom|II|2}}de .}} [[Lozère]]");
    System.out.println("Searching");
    long begin = System.currentTimeMillis();
    while (matcher.find()) {
      System.out.println("Found: " + matcher.start() + "," + matcher.end());
    }
    long end = System.currentTimeMillis();
    System.out.println("Done in " + (end - begin) + "ms");
  }*/

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

  private int undoLevels;
  private LinkedList<String> undoTexts;
  private LinkedList<String> redoTexts;
  private JButton undoButton;
  private transient ActionListener undoAction;
  private JButton redoButton;
  private transient ActionListener redoAction;

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
    this.undoLevels = 0;
    this.undoTexts = new LinkedList<String>();
    this.redoTexts = new LinkedList<String>();
    this.formatter = new MWPaneBasicFormatter();
    initialize();
  }

  /**
   * @param levels Number of undo levels.
   */
  public void setUndoLevels(int levels) {
    this.undoLevels = levels;
  }

  /**
   * Update status of Undo / Redo buttons 
   */
  private void updateUndoButtons() {
    if (undoButton != null) {
      undoButton.setEnabled(!undoTexts.isEmpty() && isModified);
    }
    if (redoButton != null) {
      redoButton.setEnabled(!redoTexts.isEmpty());
    }
  }

  /**
   * @param undo Undo button.
   */
  public void setUndoButton(JButton undo) {
    if ((undoButton != null) && (undoAction != null)) {
      undoButton.removeActionListener(undoAction);
    }
    undoButton = undo;
    if (undoButton != null) {
      undoAction = new ActionListener() {
        public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
          undo();
        }
      };
      undoButton.addActionListener(undoAction);
    }
    updateUndoButtons();
  }

  /**
   * Undo last change.
   */
  void undo() {
    if (undoTexts.isEmpty()) {
      return;
    }
    String newText = undoTexts.getLast();
    String oldText = getText();
    if (oldText.equals(newText)) {
      if (undoTexts.size() < 1) {
        return;
      }
      undoTexts.removeLast();
      newText = undoTexts.getLast();
    }
    undoTexts.removeLast();
    redoTexts.addLast(oldText);
    setTextInternal(newText, false, false);
    updateUndoButtons();
  }

  /**
   * @param redo Redo button.
   */
  public void setRedoButton(JButton redo) {
    if ((redoButton != null) && (redoAction != null)) {
      redoButton.removeActionListener(redoAction);
    }
    redoButton = redo;
    if (redoButton != null) {
      redoAction = new ActionListener() {
        public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
          redo();
        }
      };
      redoButton.addActionListener(redoAction);
    }
    updateUndoButtons();
  }

  /**
   * Redo last change.
   */
  void redo() {
    if (redoTexts.isEmpty()) {
      return;
    }
    String newText = redoTexts.getLast();
    String oldText = getText();
    if (oldText.equals(newText)) {
      if (redoTexts.size() < 1) {
        return;
      }
      redoTexts.removeLast();
      newText = redoTexts.getLast();
    }
    redoTexts.removeLast();
    undoTexts.addLast(oldText);
    setTextInternal(newText, false, false);
    updateUndoButtons();
  }

  /**
   * Memorize current text for undo / redo
   */
  private void validateCurrentText() {
    if (undoLevels <= 0) {
      return;
    }

    // Check if memorizing text is useful
    String currentText = getText();
    if (!undoTexts.isEmpty() && currentText.equals(undoTexts.getLast())) {
      return;
    }

    // Adding text
    undoTexts.addLast(currentText);
    while (undoTexts.size() > undoLevels) {
      undoTexts.removeFirst();
    }
    redoTexts.clear();
    updateUndoButtons();
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
      updateUndoButtons();
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
      public void changedUpdate(@SuppressWarnings("unused") DocumentEvent e) {
        changeDocument();
      }

      /* (non-Javadoc)
       * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event.DocumentEvent)
       */
      public void insertUpdate(@SuppressWarnings("unused") DocumentEvent e) {
        changeDocument();
      }

      /* (non-Javadoc)
       * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.DocumentEvent)
       */
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
      undoTexts.clear();
      redoTexts.clear();
    }
    if (validate) {
      validateCurrentText();
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

  /**
   * Replace all links.
   * 
   * @param from From.
   * @param to To.
   */
  public void replaceAllLinks(Page from, String to) {
    String text = getText();
    String fromText = from.getTitle();

    if (fromText.length() > 0) {
      StringBuilder expression = new StringBuilder();
      Pattern pattern = null;

      // Create the regular expression (simple form)
      expression.setLength(0);
      expression.append("\\[\\["); // [[
      expression.append("\\s*("); // Possible white characters
      PageUtilities.addPatternForTitle(expression, fromText);
      expression.append(")\\s*"); // Possible white characters
      expression.append("\\]\\]"); // ]]
      pattern = Pattern.compile(expression.toString());
      expression.setLength(0);
      expression.append("\\[\\["); // [[
      expression.append(to);
      expression.append("\\|$1\\]\\]");
      text = pattern.matcher(text).replaceAll(expression.toString());

      // Create the regular expression (complex form)
      expression.setLength(0);
      expression.append("\\[\\["); // [[
      expression.append("\\s*"); // Possible white characters
      PageUtilities.addPatternForTitle(expression, fromText);
      expression.append("\\s*"); // Possible white characters
      expression.append("\\|"); // Separator
      expression.append("([^\\|\\]]*)"); // Possible text
      expression.append("\\]\\]"); // ]]
      pattern = Pattern.compile(expression.toString());
      expression.setLength(0);
      expression.append("\\[\\["); // [[
      expression.append(to);
      expression.append("\\|$1\\]\\]");
      text = pattern.matcher(text).replaceAll(expression.toString());

      if (!text.equals(getText())) {
        setModified(true);
      }
      setTextInternal(text, false, false);
    }
  }

  /**
   * Remove all links.
   * 
   * @param from From.
   */
  public void removeAllLinks(Page from) {
    String text = getText();
    String fromText = from.getTitle();

    if (fromText.length() > 0) {
      StringBuilder expression = new StringBuilder();
      Pattern pattern = null;

      // Create the regular expression (simple form)
      expression.setLength(0);
      expression.append("\\[\\["); // [[
      expression.append("\\s*("); // Possible white characters
      PageUtilities.addPatternForTitle(expression, fromText);
      expression.append(")\\s*"); // Possible white characters
      expression.append("\\]\\]"); // ]]
      pattern = Pattern.compile(expression.toString());
      text = pattern.matcher(text).replaceAll("$1");

      // Create the regular expression (complex form)
      expression.setLength(0);
      expression.append("\\[\\["); // [[
      expression.append("\\s*"); // Possible white characters
      PageUtilities.addPatternForTitle(expression, fromText);
      expression.append("\\s*"); // Possible white characters
      expression.append("\\|"); // Separator
      expression.append("([^\\|\\]]*)"); // Possible text
      expression.append("\\]\\]"); // ]]
      pattern = Pattern.compile(expression.toString());
      text = pattern.matcher(text).replaceAll("$1");

      if (!text.equals(getText())) {
        setModified(true);
      }
      setTextInternal(text, false, false);
    }
  }

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
   * Select first occurence of text. 
   */
  public void selectFirstOccurence() {
    StyledDocument doc = getStyledDocument();
    int length = doc.getLength();
    int lastEnd = Integer.MAX_VALUE;
    for (int pos = 0; pos < length; pos = lastEnd) {
      Element run = doc.getCharacterElement(pos);
      lastEnd = run.getEndOffset();
      if (pos == lastEnd) {
        // offset + length beyond length of document, bail.
        break;
      }
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != Boolean.FALSE)) {
        select(
            MWPaneFormatter.getUUIDStartOffset(this, run),
            MWPaneFormatter.getUUIDEndOffet(this, run));
        return;
      }
    }
    for (int pos = 0; pos < length; pos = lastEnd) {
      Element run = doc.getCharacterElement(pos);
      lastEnd = run.getEndOffset();
      if (pos == lastEnd) {
        // offset + length beyond length of document, bail.
        break;
      }
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != null)) {
        select(
            MWPaneFormatter.getUUIDStartOffset(this, run),
            MWPaneFormatter.getUUIDEndOffet(this, run));
        return;
      }
    }
  }

  /**
   * Select previous occurence of text. 
   */
  public void selectPreviousOccurence() {
    StyledDocument doc = getStyledDocument();
    int lastStart = Integer.MIN_VALUE;
    for (int pos = getSelectionStart(); pos > 0; pos = lastStart) {
      Element run = doc.getCharacterElement(pos - 1);
      lastStart = run.getStartOffset();
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != Boolean.FALSE)) {
        select(
            MWPaneFormatter.getUUIDStartOffset(this, run),
            MWPaneFormatter.getUUIDEndOffet(this, run));
        return;
      }
    }
    selectLastOccurence();
  }

  /**
   * Select next occurence of text. 
   */
  public void selectNextOccurence() {
    StyledDocument doc = getStyledDocument();
    int length = doc.getLength();
    int lastEnd = Integer.MAX_VALUE;
    for (int pos = getSelectionEnd() + 1; pos < length; pos = lastEnd) {
      Element run = doc.getCharacterElement(pos);
      lastEnd = run.getEndOffset();
      if (pos == lastEnd) {
        // offset + length beyond length of document, bail.
        break;
      }
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != Boolean.FALSE)) {
        select(
            MWPaneFormatter.getUUIDStartOffset(this, run),
            MWPaneFormatter.getUUIDEndOffet(this, run));
        return;
      }
    }
    selectFirstOccurence();
  }

  /**
   * Select last occurence of text. 
   */
  public void selectLastOccurence() {
    StyledDocument doc = getStyledDocument();
    int lastStart = Integer.MIN_VALUE;
    for (int pos = doc.getLength(); pos > 0; pos = lastStart) {
      Element run = doc.getCharacterElement(pos - 1);
      lastStart = run.getStartOffset();
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != Boolean.FALSE)) {
        select(
            MWPaneFormatter.getUUIDStartOffset(this, run),
            MWPaneFormatter.getUUIDEndOffet(this, run));
        return;
      }
    }
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
    PageAnalysis pageAnalysis = new PageAnalysis(page, contents);
    formatter.format(this, pageAnalysis);

    isInInternalModification = oldState;

    if (!isInInternalModification) {
      validateCurrentText();
    }
  }

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
    List<Font> possibleFonts = new ArrayList<Font>();
    Font[] allFonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAllFonts();
    for (int i = 0; i < allFonts.length; i++) {
      if (allFonts[i].canDisplayUpTo(text) == -1) {
        possibleFonts.add(allFonts[i]);
      }
    }
    return possibleFonts;
  }

  // =========================================================================
  // Check Box management
  // =========================================================================

  private JCheckBox chkAddNote;
  private JCheckBox chkCreateDabWarning;
  private JCheckBox chkUpdateDabWarning;

  /**
   * @return JCheckBox used for adding a note in the Talk page.
   */
  public JCheckBox getCheckBoxAddNote() {
    return chkAddNote;
  }

  /**
   * @param chk JCheckBox used for adding a note in the Talk page.
   */
  public void setCheckBoxAddNote(JCheckBox chk) {
    chkAddNote = chk;
  }

  /**
   * @return JCheckBox used for creating disambiguation warning in the Talk page.
   */
  public JCheckBox getCheckBoxCreateDabWarning() {
    return chkCreateDabWarning;
  }

  /**
   * @param chk JCheckBox used for creating disambiguation warning in the Talk page.
   */
  public void setCheckBoxCreateDabWarning(JCheckBox chk) {
    chkCreateDabWarning = chk;
  }

  /**
   * @return JCheckBox used for updating disambiguation warning in the Talk page.
   */
  public JCheckBox getCheckBoxUpdateDabWarning() {
    return chkUpdateDabWarning;
  }

  /**
   * @param chk JCheckBox used for updating disambiguation warning in the Talk page.
   */
  public void setCheckBoxUpdateDabWarning(JCheckBox chk) {
    chkUpdateDabWarning = chk;
  }

  // =========================================================================
  // Complex Pane management
  // =========================================================================

  private TitleTreeManager treeManager;

  /**
   * A tree node for titles. 
   */
  private static class TitleTreeNode extends DefaultMutableTreeNode {

    private static final long serialVersionUID = 1L;

    private final PageElementTitle title;
    private int level;

    /**
     * @param title Title.
     */
    public TitleTreeNode(PageElementTitle title) {
      super((title != null) ? title : "Page");
      this.title = title;
      this.level = (title != null) ? title.getFirstLevel() : 0;
    }

    /**
     * @return Title level.
     */
    public int getInitialTitleLevel() {
      if (title != null) {
        return title.getFirstLevel();
      }
      return 0;
    }

    /**
     * @return Title level.
     */
    public int getCurrentTitleLevel() {
      return level;
    }

    /**
     * @param level Title level.
     */
    public void setCurrentTitleLevel(int level) {
      this.level = level;
    }

    /**
     * @return Title.
     */
    public PageElementTitle getTitle() {
      return title;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.DefaultMutableTreeNode#toString()
     */
    @Override
    public String toString() {
      if (title == null) {
        return super.toString();
      }
      StringBuilder buffer = new StringBuilder();
      buffer.append("(");
      buffer.append(title.getFirstLevel() - 1);
      if (title.getFirstLevel() != level) {
        buffer.append(" -> ");
        buffer.append(level - 1);
      }
      buffer.append(") ");
      buffer.append(title.getTitle());
      return buffer.toString();
    }
  }

  /**
   * A manager for the tree of titles.
   */
  private static class TitleTreeManager implements TreeSelectionListener, ActionListener {

    private final MWPane textPane;
    private final JSplitPane splitPane;
    private final JTree treeToc;
    private final DefaultTreeModel modelToc;
    private final JTree treeToc2;
    private final DefaultTreeModel modelToc2;

    private boolean tocIsDisplayed;

    /**
     * Create the title tree manager.
     * 
     * @param textPane Text pane.
     */
    public TitleTreeManager(MWPane textPane) {

      // Text pane
      this.textPane = textPane;
      splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
      JScrollPane scrollContents = new JScrollPane(textPane);
      scrollContents.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      splitPane.setBottomComponent(scrollContents);

      // Table of contents
      JPanel panelTOC = new JPanel(new GridBagLayout());
      GridBagConstraints constraints = new GridBagConstraints();
      constraints.fill = GridBagConstraints.BOTH;
      constraints.gridheight = 1;
      constraints.gridwidth = 1;
      constraints.gridx = 0;
      constraints.gridy = 0;
      constraints.insets = new Insets(0, 0, 0, 0);
      constraints.ipadx = 0;
      constraints.ipady = 0;
      constraints.weightx = 1;
      constraints.weighty = 1;

      // Toolbar
      JToolBar toolbarButtons = new JToolBar(SwingConstants.VERTICAL);
      toolbarButtons.setFloatable(false);
      JButton buttonLess = Utilities.createJButton(
          "gnome-go-previous.png", EnumImageSize.NORMAL,
          GT._("Decrement title level"), false);
      buttonLess.setActionCommand("-");
      buttonLess.addActionListener(this);
      toolbarButtons.add(buttonLess);
      JButton buttonMore = Utilities.createJButton(
          "gnome-go-next.png", EnumImageSize.NORMAL,
          GT._("Increment title level"), false);
      buttonMore.setActionCommand("+");
      buttonMore.addActionListener(this);
      toolbarButtons.add(buttonMore);
      JButton buttonDone = Utilities.createJButton(
          "commons-approve-icon.png", EnumImageSize.NORMAL,
          GT._("Validate the new table of contents"), false);
      buttonDone.setActionCommand("OK");
      buttonDone.addActionListener(this);
      toolbarButtons.add(buttonDone);
      constraints.weightx = 0;
      panelTOC.add(toolbarButtons, constraints);
      constraints.gridx++;

      // Tree node renderer
      DefaultTreeCellRenderer rendererToc = new DefaultTreeCellRenderer();
      rendererToc.setLeafIcon(rendererToc.getClosedIcon());

      // Table of contents Tree
      TitleTreeNode rootToc = new TitleTreeNode(null);
      modelToc = new DefaultTreeModel(rootToc);
      treeToc = new JTree(modelToc);
      treeToc.setRootVisible(false);
      treeToc.setShowsRootHandles(true);
      treeToc.getSelectionModel().setSelectionMode(
          TreeSelectionModel.SINGLE_TREE_SELECTION);
      treeToc.setCellRenderer(rendererToc);
      treeToc.addTreeSelectionListener(this);
      JScrollPane scrollTree = new JScrollPane(treeToc);
      scrollTree.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      constraints.weightx = 1;
      panelTOC.add(scrollTree, constraints);
      constraints.gridx++;

      // Second tree
      TitleTreeNode rootToc2 = new TitleTreeNode(null);
      modelToc2 = new DefaultTreeModel(rootToc2);
      treeToc2 = new JTree(modelToc2);
      treeToc2.setRootVisible(false);
      treeToc2.setShowsRootHandles(true);
      treeToc2.setCellRenderer(rendererToc);
      JScrollPane scrollTree2 = new JScrollPane(treeToc2);
      scrollTree2.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      constraints.weightx = 1;
      panelTOC.add(scrollTree2, constraints);
      constraints.gridx++;
      
      splitPane.setTopComponent(panelTOC);

      // Hide table of contents
      hideToc();
    }

    /**
     * @return Component containing the trees.
     */
    public JComponent getComponent() {
      return splitPane;
    }

    /**
     * Toggle display of table of contents.
     */
    public void toggleToc() {
      if (tocIsDisplayed) {
        hideToc();
      } else {
        displayToc();
      }
    }

    /**
     * Dispay table of contents.
     */
    public void displayToc() {
      if (!tocIsDisplayed) {
        updateTreeToc();
        updateTreeToc2();
        selectTreeToc2();
        tocIsDisplayed = true;
      }
      splitPane.setDividerLocation(200);
      splitPane.setDividerSize(2);
      splitPane.setResizeWeight(0.0);
      textPane.setEditableInternal(false);
    }

    /**
     * Hide table of contents.
     */
    public void hideToc() {
      tocIsDisplayed = false;
      splitPane.setDividerLocation(0);
      splitPane.setDividerSize(0);
      splitPane.setResizeWeight(0.0);
      textPane.setEditableInternal(textPane.isEditable);
    }

    /* (non-Javadoc)
     * @see javax.swing.event.TreeSelectionListener#valueChanged(javax.swing.event.TreeSelectionEvent)
     */
    public void valueChanged(@SuppressWarnings("unused") TreeSelectionEvent e) {
      TitleTreeNode treeNode = (TitleTreeNode) treeToc.getLastSelectedPathComponent();
      if (treeNode == null) {
        return;
      }
      Object nodeInfo = treeNode.getUserObject();
      if (nodeInfo instanceof PageElementTitle) {
        PageElementTitle title = (PageElementTitle) nodeInfo;
        try {
          textPane.setCaretPosition(title.getBeginIndex());
          textPane.moveCaretPosition(title.getEndIndex());
        } catch (IllegalArgumentException e2) {
          //
        }
        textPane.requestFocusInWindow();
      }
      selectTreeToc2();
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
      if (treeToc == null) {
        return;
      }
      TitleTreeNode treeNode = (TitleTreeNode) treeToc.getLastSelectedPathComponent();
      if (treeNode == null) {
        return;
      }
      if ("+".equals(e.getActionCommand())) {
        changeTitleLevel(treeNode, 1);
        updateTreeToc2();
        selectTreeToc2();
      } else if ("-".equals(e.getActionCommand())) {
        changeTitleLevel(treeNode, -1);
        updateTreeToc2();
        selectTreeToc2();
      } else if ("OK".equals(e.getActionCommand())) {
        StringBuilder contents = new StringBuilder(textPane.getText());
        applyChanges(contents, treeToc.getModel().getRoot());
        textPane.changeText(contents.toString());
      }
      treeToc.repaint();
      textPane.requestFocusInWindow();
    }

    /**
     * Update table of contents tree.
     */
    private void updateTreeToc() {
      String contents = textPane.getText();
      int currentIndex = 0;
      TitleTreeNode rootNode = new TitleTreeNode(null);
      TitleTreeNode lastNode = rootNode;
      Collection<PageElementComment> comments = PageContents.findAllComments(
          textPane.getWikipedia(), contents);
      while ((currentIndex < contents.length())) {
        PageElementTitle title = PageContents.findNextTitle(
            textPane.getWikipedia(), contents, currentIndex, comments);
        if (title == null) {
          currentIndex = contents.length();
        } else {
          while ((lastNode != null) &&
                 (lastNode.getInitialTitleLevel() >= title.getFirstLevel())) {
            if (lastNode.getParent() != null) {
              lastNode = (TitleTreeNode) lastNode.getParent();
            } else {
              lastNode = null;
            }
          }
          if (lastNode == null) {
            lastNode = rootNode;
          }
          TitleTreeNode tmpNode = new TitleTreeNode(title);
          lastNode.add(tmpNode);
          lastNode = tmpNode;
          currentIndex = title.getEndIndex();
        }
      }
      modelToc.setRoot(rootNode);
    }

    /**
     * Update table of contents second tree. 
     */
    private void updateTreeToc2() {
      TitleTreeNode rootNode2 = new TitleTreeNode(null);
      TitleTreeNode rootNode = (TitleTreeNode) modelToc.getRoot();
      updateTreeToc2Node(rootNode2, rootNode);
      modelToc2.setRoot(rootNode2);
    }

    /**
     * Select node in the second tree depending on the selection in the first tree.
     */
    private void selectTreeToc2() {
      TitleTreeNode treeNode = (TitleTreeNode) treeToc.getLastSelectedPathComponent();
      if (treeNode == null) {
        treeToc2.setSelectionRows(null);
      } else {
        TitleTreeNode otherNode = findTreeNode(
            (TitleTreeNode) modelToc2.getRoot(),
            treeNode.getTitle());
        if (otherNode != null) {
          TreePath treePath = new TreePath(otherNode.getPath());
          treeToc2.scrollPathToVisible(treePath);
          treeToc2.setSelectionPath(treePath);
        }
      }
    }

    /**
     * Find a tree node matching a title.
     * 
     * @param node Current node.
     * @param title Title.
     * @return Node matching the title if found.
     */
    private TitleTreeNode findTreeNode(TitleTreeNode node, PageElementTitle title) {
      if (node == null) {
        return null;
      }
      if (title == node.getTitle()) {
        return node;
      }
      for (int i = 0; i < node.getChildCount(); i++) {
        TitleTreeNode found = findTreeNode((TitleTreeNode) node.getChildAt(i), title);
        if (found != null) {
          return found;
        }
      }
      return null;
    }

    /**
     * Update table of contents second tree for a node and its children.
     * 
     * @param rootNode2 Root of second tree.
     * @param node Current node to add.
     */
    private void updateTreeToc2Node(TitleTreeNode rootNode2, TitleTreeNode node) {
      if (node == null) {
        return;
      }
      for (int i = 0; i < node.getChildCount(); i++) {
        TitleTreeNode currentNode = (TitleTreeNode) node.getChildAt(i);
        TitleTreeNode newNode = new TitleTreeNode(currentNode.getTitle());
        newNode.setCurrentTitleLevel(currentNode.getCurrentTitleLevel());
        TitleTreeNode parentNode = (TitleTreeNode) rootNode2.getLastLeaf();
        while ((parentNode.isRoot() == false) &&
               (parentNode.getCurrentTitleLevel() >= newNode.getCurrentTitleLevel())) {
          parentNode = (TitleTreeNode) parentNode.getParent();
        }
        parentNode.add(newNode);
        updateTreeToc2Node(rootNode2, currentNode);
      }
    }

    /**
     * Change title level (including children).
     * 
     * @param treeNode Node.
     * @param increment Increment.
     */
    private void changeTitleLevel(TitleTreeNode treeNode, int increment) {
      if (treeNode.getCurrentTitleLevel() + increment > 0) {
        treeNode.setCurrentTitleLevel(treeNode.getCurrentTitleLevel() + increment);
        for (int i = 0; i < treeNode.getChildCount(); i++) {
          TreeNode child = treeNode.getChildAt(i);
          if (child instanceof TitleTreeNode) {
            changeTitleLevel((TitleTreeNode) treeNode.getChildAt(i), increment);
          }
        }
      }
    }

    /**
     * Save changes to table of contents.
     * 
     * @param contents Contents.
     * @param node Node.
     */
    private void applyChanges(StringBuilder contents, Object node) {
      if ((contents == null) || (node == null)) {
        return;
      }
      if (!(node instanceof TitleTreeNode)) {
        return;
      }
      TitleTreeNode treeNode = (TitleTreeNode) node;
      for (int i = treeNode.getChildCount(); i > 0; i--) {
        applyChanges(contents, treeNode.getChildAt(i - 1));
      }
      if (treeNode.getCurrentTitleLevel() != treeNode.getInitialTitleLevel()) {
        StringBuilder newTitle = new StringBuilder();
        for (int i = 0; i < treeNode.getCurrentTitleLevel(); i++) {
          newTitle.append("=");
        }
        newTitle.append(" ");
        newTitle.append(treeNode.getTitle().getTitle());
        newTitle.append(" ");
        for (int i = 0; i < treeNode.getCurrentTitleLevel(); i++) {
          newTitle.append("=");
        }
        contents.replace(
            treeNode.getTitle().getBeginIndex(),
            treeNode.getTitle().getEndIndex(),
            newTitle.toString());
        textPane.hideToc();
        textPane.resetAttributes();
        textPane.requestFocusInWindow();
      }
    }
  }

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
      textPane.treeManager = new TitleTreeManager(textPane);
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
   */
  public void displayToc() {
    if (treeManager != null) {
      treeManager.displayToc();
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
