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

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageUtilities;
import org.wikipediacleaner.api.data.TemplateMatch;
import org.wikipediacleaner.api.data.TemplateParameter;
import org.wikipediacleaner.gui.swing.action.FindTextAction;
import org.wikipediacleaner.gui.swing.action.ReplaceLinkAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * A text component to colorize / edit MediaWiki text.
 */
public class MediaWikiPane
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
  private ArrayList<Page> internalLinks;

  private static final KeyStroke lastLinkKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_L, InputEvent.CTRL_MASK);
  private static final KeyStroke lastReplaceKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_R, InputEvent.CTRL_MASK);

  private boolean isModified = false;
  boolean isInInternalModification = false;

  private transient MediaWikiPopupListener popupListener;

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
  public MediaWikiPane(EnumWikipedia wikipedia, Page page, BasicWindow window) {
    super();
    this.wikipedia = wikipedia;
    this.page = page;
    this.window = window;
    this.undoLevels = 0;
    this.undoTexts = new LinkedList<String>();
    this.redoTexts = new LinkedList<String>();
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
    setTextModified(newText, false, false);
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
    setTextModified(newText, false, false);
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
  public void setPage(Page page) {
    this.page = page;
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
   * @param chk JCheckBox used to forcing adding a note in the Talk page.
   */
  public void setCheckBoxAddNote(JCheckBox chk) {
    if (popupListener != null) {
      popupListener.setCheckBoxAddNote(chk);
    }
  }

  /**
   * Initialize styles. 
   */
  private void initialize() {
    boolean oldState = isInInternalModification;
    isInInternalModification = true;
    this.setComponentOrientation(wikipedia.getComponentOrientation());
    DefaultStyledDocument doc = new DefaultStyledDocument();
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

    Style root = StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);

    Style normalLink = addStyle(MediaWikiConstants.STYLE_NORMAL_LINK, root);
    StyleConstants.setBold(normalLink, true);
    StyleConstants.setForeground(normalLink, Color.BLUE);
    normalLink.addAttribute(MediaWikiConstants.ATTRIBUTE_TYPE, MediaWikiConstants.VALUE_NORMAL_LINK);

    Style normalTemplate = addStyle(MediaWikiConstants.STYLE_NORMAL_TEMPLATE, root);
    StyleConstants.setBold(normalTemplate, true);
    StyleConstants.setForeground(normalTemplate, Color.BLUE);
    normalTemplate.addAttribute(MediaWikiConstants.ATTRIBUTE_TYPE, MediaWikiConstants.VALUE_NORMAL_TEMPLATE);

    Style disambiguationLink = addStyle(MediaWikiConstants.STYLE_DISAMBIGUATION_LINK, root);
    StyleConstants.setBold(disambiguationLink, true);
    StyleConstants.setForeground(disambiguationLink, Color.RED);
    disambiguationLink.addAttribute(MediaWikiConstants.ATTRIBUTE_TYPE, MediaWikiConstants.VALUE_DISAMBIGUATION_LINK);

    Style disambiguationTemplate = addStyle(MediaWikiConstants.STYLE_DISAMBIGUATION_TEMPLATE, root);
    StyleConstants.setBold(disambiguationTemplate, true);
    StyleConstants.setForeground(disambiguationTemplate, Color.RED);
    disambiguationTemplate.addAttribute(MediaWikiConstants.ATTRIBUTE_TYPE, MediaWikiConstants.VALUE_DISAMBIGUATION_TEMPLATE);

    Style helpRequestedLink = addStyle(MediaWikiConstants.STYLE_HELP_REQUESTED_LINK, root);
    StyleConstants.setBold(helpRequestedLink, true);
    StyleConstants.setForeground(helpRequestedLink, Color.ORANGE);
    helpRequestedLink.addAttribute(MediaWikiConstants.ATTRIBUTE_TYPE, MediaWikiConstants.VALUE_HELP_REQUESTED_LINK);

    Style redirectLink = addStyle(MediaWikiConstants.STYLE_REDIRECT_LINK, root);
    StyleConstants.setBold(redirectLink, true);
    StyleConstants.setItalic(redirectLink, true);
    StyleConstants.setForeground(redirectLink, Color.CYAN);
    redirectLink.addAttribute(MediaWikiConstants.ATTRIBUTE_TYPE, MediaWikiConstants.VALUE_REDIRECT_LINK);

    Style missingLink = addStyle(MediaWikiConstants.STYLE_MISSING_LINK, root);
    StyleConstants.setBold(missingLink, true);
    StyleConstants.setForeground(missingLink, Color.ORANGE);
    StyleConstants.setStrikeThrough(missingLink, true);
    missingLink.addAttribute(MediaWikiConstants.ATTRIBUTE_TYPE, MediaWikiConstants.VALUE_MISSING_LINK);

    Style externalLink = addStyle(MediaWikiConstants.STYLE_EXTERNAL_LINK, root);
    StyleConstants.setForeground(externalLink, new Color(128, 128, 255));
    externalLink.addAttribute(MediaWikiConstants.ATTRIBUTE_TYPE, MediaWikiConstants.VALUE_EXTERNAL_LINK);

    ActionMap actionMap = getActionMap();
    InputMap inputMapFocused = getInputMap();
    InputMap inputMapInFocused = getInputMap(WHEN_IN_FOCUSED_WINDOW);
    KeyStroke keyStroke = null;

    popupListener = new MediaWikiPopupListener(wikipedia, window);
    addKeyListener(popupListener);
    addMouseListener(popupListener);
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

  /* (non-Javadoc)
   * @see javax.swing.JEditorPane#setText(java.lang.String)
   */
  @Override
  public void setText(String t) {
    setTextModified(t, true, true);
  }

  /**
   * Enabling changing text without resetting the modified flag.
   * 
   * @param t New text.
   * @param resetModified Flag indicating if the modified flag should be reseted.
   */
  private void setTextModified(
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
      setTextModified(text, false, false);
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
      setTextModified(text, false, false);
    }
  }

  /**
   * @return List of disambiguation links.
   */
  public ArrayList<Page> getInternalLinks() {
    return internalLinks;
  }

  /**
   * @param list List of disambiguation links.
   */
  public void setInternalLinks(ArrayList<Page> list) {
    internalLinks = list;
    resetAttributes();
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
          (attr.getAttribute(MediaWikiConstants.ATTRIBUTE_TYPE) != null)) {
        select(run.getStartOffset(), run.getEndOffset());
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
          (attr.getAttribute(MediaWikiConstants.ATTRIBUTE_TYPE) != null)) {
        select(run.getStartOffset(), run.getEndOffset());
        return;
      }
    }
  }

  /**
   * Reset attributes of the document.
   * This method should be called after modifications are done.
   */
  public void resetAttributes() {

    boolean oldState = isInInternalModification;
    isInInternalModification = true;

    // First remove MediaWiki styles
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
      if ((attr != null) && (attr.getAttribute(MediaWikiConstants.ATTRIBUTE_TYPE) != null)) {
        doc.setCharacterAttributes(
            run.getStartOffset(),
            run.getEndOffset() - run.getStartOffset(),
            getStyle(StyleContext.DEFAULT_STYLE),
            true);
      }
    }

    int startPosition = Integer.MAX_VALUE;
    int endPosition = Integer.MAX_VALUE;
    int secondStartPosition = Integer.MAX_VALUE;
    int secondEndPosition = Integer.MAX_VALUE;
    int thirdStartPosition = Integer.MAX_VALUE;
    int thirdEndPosition = Integer.MAX_VALUE;

    // Look for disambiguation links
    if (internalLinks != null) {
      for (int i = 0; i < internalLinks.size(); i++) {
        Page link = internalLinks.get(i);
        Pattern pattern = PageUtilities.createPatternForInternalLink(link);
        Matcher matcher = pattern.matcher(getText());
        while (matcher.find()) {
          int start = matcher.start();
          int end = matcher.end();
          boolean disambiguation = Boolean.TRUE.equals(link.isDisambiguationPage());
          Style attr = getStyle(disambiguation ?
              MediaWikiConstants.STYLE_DISAMBIGUATION_LINK :
              link.isRedirect() ?
                  MediaWikiConstants.STYLE_REDIRECT_LINK :
                  link.isExisting() ?
                      MediaWikiConstants.STYLE_NORMAL_LINK :
                      MediaWikiConstants.STYLE_MISSING_LINK);
          attr = (Style) attr.copyAttributes();
          attr.addAttribute(MediaWikiConstants.ATTRIBUTE_PAGE, link);
          String text = matcher.group(matcher.groupCount());
          if (text == null) {
            text = matcher.group(1);
            if (matcher.group(matcher.groupCount() - 1) != null) {
              text = text.replaceAll("\\s*\\(.*\\)\\s*$", "");
            }
          }
          attr.addAttribute(MediaWikiConstants.ATTRIBUTE_TEXT, text);
          doc.setCharacterAttributes(start, end - start, attr, true);
          if (start < startPosition) {
            startPosition = start;
            endPosition = end;
          }
        }
      }
    }

    // Analyze templates
    if (internalLinks != null) {
      for (int numT = 0; numT < wikipedia.getDisambiguationMatchesCount(); numT++) {
        TemplateMatch template = wikipedia.getDisambiguationMatch(numT);
        Pattern pattern = PageUtilities.createPatternForTemplate(template);
        Matcher matcher = pattern.matcher(getText());
        while (matcher.find()) {
          int start = matcher.start();
          int end = matcher.end();
          ArrayList<TemplateParameter> parameters = PageUtilities.analyzeTemplateParameters(template, matcher, page);
          if (parameters != null) {
            for (TemplateParameter param : parameters) {
              // Analyze each page
              if (param.isRelevant()) {
                for (int i = 0; i < internalLinks.size(); i++) {
                  Page link = internalLinks.get(i);
                  if (Page.areSameTitle(link.getTitle(), param.getValue())) {
                    String styleName = null;
                    if (template.isGood() || !Boolean.FALSE.equals(link.isDisambiguationPage())) {
                      styleName = MediaWikiConstants.STYLE_NORMAL_TEMPLATE;
                    } else {
                      if (template.isHelpNeeded()) {
                        styleName = MediaWikiConstants.STYLE_HELP_REQUESTED_LINK;
                      } else {
                        styleName = MediaWikiConstants.STYLE_DISAMBIGUATION_TEMPLATE;
                      }
                    }
                    Style attr = getStyle(styleName);
                    attr = (Style) attr.copyAttributes();
                    attr.addAttribute(MediaWikiConstants.ATTRIBUTE_PAGE, link);
                    if ((template.isHelpNeeded()) && (parameters.size() > 0)) {
                      attr.addAttribute(
                          MediaWikiConstants.ATTRIBUTE_TEXT,
                          (parameters.size() > 1) ? parameters.get(1).getValue() : parameters.get(0).getValue());
                    }
                    doc.setCharacterAttributes(start, end - start, attr, true);
                    if (template.isGood() || !Boolean.FALSE.equals(link.isDisambiguationPage())) {
                      if (start < thirdStartPosition) {
                        thirdStartPosition = start;
                        thirdEndPosition = end;
                      }
                    } else if (template.isHelpNeeded()) {
                      if (start < secondStartPosition) {
                        secondStartPosition = start;
                        secondEndPosition = end;
                      }
                    } else {
                      if (start < startPosition) {
                        startPosition = start;
                        endPosition = end;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    // Move caret to force first element to be visible
    if (startPosition < Integer.MAX_VALUE) {
      setCaretPosition(startPosition);
      moveCaretPosition(endPosition);
    } else if (secondStartPosition < Integer.MAX_VALUE) {
      setCaretPosition(secondStartPosition);
      moveCaretPosition(secondEndPosition);
    } else if (thirdStartPosition < Integer.MAX_VALUE) {
      setCaretPosition(thirdStartPosition);
      moveCaretPosition(thirdEndPosition);
    }

    isInInternalModification = oldState;

    if (!isInInternalModification) {
      validateCurrentText();
    }
  }

  /**
   * Retrieve current chapter hierarchy.
   * 
   * @param position Position in the text.
   * @return Chapters.
   */
  LinkedList<String> getChapterPosition(int position) {
    LinkedList<String> chapters = null;
    try {
      int currentLevel = Integer.MAX_VALUE;
      while ((position >= 0) && (currentLevel > 1)) {

        // Retrieving paragraph
        Element paragraph = getStyledDocument().getParagraphElement(position);
        int start = paragraph.getStartOffset();
        int end = paragraph.getEndOffset();
        position = start - 1;

        // Analyzing text
        String value = getText(start, end - start);
        boolean falseComment = false;
        while (((start = value.indexOf("<!--")) > 0) && !falseComment) {
          //start = value.indexOf("<!--");
          end = value.indexOf("-->", start + 4);
          if ((start != -1) && (end != -1)) {
            end += 3;
            if (end < value.length() - 1) {
              value = value.substring(0, start) + value.substring(end);
            } else {
              value = value.substring(0, start);
            }
          } else {
            falseComment = true;
          }
        }
        start = 0;
        end = value.length() - 1;
        while ((end > 0) && Character.isWhitespace(value.charAt(end))) {
          end--;
        }
        int level = 0;
        while ((start < end - 2) && (value.charAt(start) == '=') && (value.charAt(end) == '=')) {
          level++;
          start++;
          end--;
        }
        if ((level > 0) && (level < currentLevel)) {
          currentLevel = level;
          if (chapters == null) {
            chapters = new LinkedList<String>();
          }
          chapters.add(0, "" + currentLevel + " - " + value.substring(start, end + 1).trim());
        }
      }
    } catch (BadLocationException e) {
      //
    }
    return chapters;
  }
}
