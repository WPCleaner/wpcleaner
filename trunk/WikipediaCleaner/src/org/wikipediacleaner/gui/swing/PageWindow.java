/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import org.wikipediacleaner.api.MediaWikiController;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.action.ReplaceAllLinksAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MediaWikiPane;
import org.wikipediacleaner.gui.swing.worker.SendWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;

/**
 * A base class for Wikipedia Cleaner windows with page contents.
 */
public abstract class PageWindow
  extends BasicWindow
  implements ActionListener, ItemListener {

  private Page page;
  private String pageName;

  private boolean pageLoaded = false;

  /**
   * @return Page.
   */
  public Page getPage() {
    return page;
  }

  /**
   * @param page Page.
   */
  protected void setPage(Page page) {
    this.page = page;
  }

  /**
   * @return Page name.
   */
  protected String getPageName() {
    if ((pageName == null) && (page != null)) {
      return page.getTitle();
    }
    return pageName;
  }

  /**
   * @param pageName Page name.
   */
  protected void setPageName(String pageName) {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia != null) {
      this.pageName = wikipedia.normalizeTitle(pageName);
    } else {
      this.pageName = pageName;
    }
  }

  /**
   * @return Page name in the text component.
   */
  protected String getTextPageName() {
    if (textPagename != null) {
      return textPagename.getText();
    }
    return pageName;
  }

  /**
   * @return Flag indicating if the page is loaded.
   */
  protected boolean isPageLoaded() {
    return pageLoaded;
  }

  /**
   * Indicates that the page is loaded. 
   */
  protected void setPageLoaded() {
    pageLoaded = true;
  }

  /**
   * Indicates if the page is loaded.
   * 
   * @param loaded Flag indicating if the page is loaded.
   */
  protected void setPageLoaded(boolean loaded) {
    pageLoaded = loaded;
  }

  /**
   * @return Default comment.
   */
  protected String getDefaultComment() {
    return getWikipedia().getUpdatePageMessage();
  }

  /**
   * @param comment Comment.
   */
  protected void setComment(String comment) {
    if (textComment == null) {
      return;
    }
    if ((chkAutomaticComment != null) && (!chkAutomaticComment.isSelected())) {
      return;
    }
    textComment.setText(comment);
  }

  /**
   * Clean page. 
   */
  protected void clean() {
    pageLoaded = false;
    if (getTextContents() != null) {
      getTextContents().setText(null);
    }
    page = DataManager.getPage(getWikipedia(), getTextPageName(), null, null);
    updateComponentState();
  }

  /* ====================================================================== */
  /* Workers                                                                */
  /* ====================================================================== */

  /**
   * Setup the Reloade Worker.
   * 
   * @param reloadWorker Reload Worker.
   */
  protected void setupReloadWorker(BasicWorker reloadWorker) {
    if (reloadWorker == null) {
      return;
    }
    reloadWorker.setListener(new DefaultBasicWorkerListener() {
      @Override
      public void beforeStart(
          @SuppressWarnings("unused") BasicWorker worker) {
        beforeStartReloadWorker();
      }
      @Override
      public void beforeFinished(
          @SuppressWarnings("unused") BasicWorker worker) {
        beforeFinishedReloadWorker();
      }
      @Override
      public void afterFinished(
          @SuppressWarnings("unused") BasicWorker worker,
          @SuppressWarnings("unused") boolean ok) {
        afterFinishedReloadWorker();
      }
    });
  }

  /**
   * Callback called at the end of the Reload Worker.
   */
  protected void afterFinishedReloadWorker() {
    setContents();
    updateComponentState();
  }

  /**
   * Callback called before the end of the Reload Worker.
   */
  protected void beforeFinishedReloadWorker() {
    setPageLoaded();
  }

  /**
   * Callback called before the start of the Reload Worker. 
   */
  protected void beforeStartReloadWorker() {
    //
  }

  /* ====================================================================== */
  /* Components                                                             */
  /* ====================================================================== */

  private JLabel textPagename;
  private JButton buttonDisambiguation;
  private JButton buttonDisambiguationRedirect;
  private JButton buttonFullAnalysisRedirect;
  private JButton buttonRedo;
  private JButton buttonUndo;
  private JButton buttonReload;
  private JButton buttonSend;
  private JButton buttonView;
  private JButton buttonViewHistory;
  private JButton buttonWatch;
  private JButton buttonFullAnalysis;
  private JTextField textComment;
  private JCheckBox chkAutomaticComment;
  JCheckBox chkCloseAfterSend;
  private JCheckBox chkEditTalkPage;
  JCheckBox chkUpdateDabWarning;
  JCheckBox chkCreateDabWarning;
  private JLabel lblLastModified;
  private JLabel lblEditProtectionLevel;

  private MediaWikiPane textContents;

  /**
   * Update component state.
   */
  @Override
  protected void updateComponentState() {
    boolean redirect = (page != null) && (page.isRedirect());
    boolean article = (page != null) && (page.isArticle());
    if (textContents != null) {
      textContents.setEnabled(pageLoaded);
    }
    if (buttonView != null) {
      buttonView.setEnabled(pageLoaded);
    }
    if (buttonViewHistory != null) {
      buttonViewHistory.setEnabled(pageLoaded);
    }
    if ((buttonSend != null) && (textContents != null)) {
      buttonSend.setEnabled(pageLoaded && textContents.isModified());
    }
    if (buttonDisambiguation != null) {
      buttonDisambiguation.setEnabled(pageLoaded);
    }
    if (buttonFullAnalysis != null) {
      buttonFullAnalysis.setEnabled(pageLoaded);
    }
    if (buttonWatch != null) {
      buttonWatch.setEnabled(pageLoaded);
    }
    if (buttonFullAnalysisRedirect != null) {
      buttonFullAnalysisRedirect.setEnabled(redirect);
      buttonFullAnalysisRedirect.setVisible(redirect);
    }
    if (buttonDisambiguationRedirect != null) {
      buttonDisambiguationRedirect.setEnabled(redirect);
      buttonDisambiguationRedirect.setVisible(redirect);
    }
    if (chkCloseAfterSend != null) {
      chkCloseAfterSend.setEnabled(pageLoaded);
    }
    if (chkEditTalkPage != null) {
      chkEditTalkPage.setEnabled(pageLoaded && article);
    }
    boolean dabWarning =
      article &&
      (getWikipedia().getDisambiguationWarningTemplate() != null) &&
      (getWikipedia().getTodoTemplates() != null);
    if (chkUpdateDabWarning != null) {
      chkUpdateDabWarning.setEnabled(pageLoaded && dabWarning);
    }
    if (chkCreateDabWarning != null) {
      chkCreateDabWarning.setEnabled(
          pageLoaded && dabWarning &&
          (chkUpdateDabWarning != null) &&
          (chkUpdateDabWarning.isSelected()));
    }
    if ((textComment != null) && (chkAutomaticComment != null)) {
      textComment.setEnabled(!chkAutomaticComment.isSelected());
    }
    if (menuFixRedirects != null) {
      menuFixRedirects.setEnabled(menuFixRedirects.getItemCount() > 0);
    }
  }

  /**
   * Add a component for the page name.
   * 
   * @param panel Container.
   */
  protected void addTextPageName(JPanel panel) {
    if (textPagename == null) {
      textPagename = new JLabel(getPageName());
      JLabel labelPagename = Utilities.createJLabel(GT._("&Page :"));
      labelPagename.setLabelFor(textPagename);
      labelPagename.setHorizontalAlignment(SwingConstants.TRAILING);
      panel.add(labelPagename);
      panel.add(textPagename);
    }
  }

  /**
   * Add a component for the Disambiguation button.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonDisambiguation(JComponent panel, boolean icon) {
    if (buttonDisambiguation == null) {
      if (icon) {
        buttonDisambiguation = Utilities.createJButton(
            "commons-disambig-colour.png", EnumImageSize.NORMAL,
            GT._("Disambiguation"), false);
      } else {
        buttonDisambiguation = Utilities.createJButton(GT._("Disambiguation"));
      }
      buttonDisambiguation.setActionCommand(ACTION_DISAMBIGUATION_PAGE);
      buttonDisambiguation.addActionListener(this);
      panel.add(buttonDisambiguation);
    }
  }

  /**
   * Create a First Occurence button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return First Occurence button.
   */
  public JButton createButtonFirstOccurence(ActionListener listener, boolean icon) {
    JButton button = null;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-go-first.png", EnumImageSize.NORMAL,
          GT._("First occurrence (Alt + &F)"), false);
    } else {
      button = Utilities.createJButton(GT._("&First occurrence"));
    }
    button.setActionCommand(ACTION_FIRST_OCCURENCE);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Add a component for the Full Analysis button.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonFullAnalysis(JComponent panel, boolean icon) {
    if (buttonFullAnalysis == null) {
      buttonFullAnalysis = createButtonFullAnalysis(this, icon);
      panel.add(buttonFullAnalysis);
    }
  }

  /**
   * Create a Full Analysis button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return Full Analysis button.
   */
  public JButton createButtonFullAnalysis(ActionListener listener, boolean icon) {
    JButton button = null;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-system-run.png", EnumImageSize.NORMAL,
          GT._("Full analysis"), false);
    } else {
      button = Utilities.createJButton(GT._("Full analysis"));
    }
    button.setActionCommand(ACTION_FULL_ANALYSIS_PAGE);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Create a Last Occurence button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return Last Occurence button.
   */
  public JButton createButtonLastOccurence(ActionListener listener, boolean icon) {
    JButton button = null;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-go-last.png", EnumImageSize.NORMAL,
          GT._("Last occurrence (Alt + &L)"), false);
    } else {
      button = Utilities.createJButton(GT._("&Last occurrence"));
    }
    button.setActionCommand(ACTION_LAST_OCCURENCE);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Create a Next Occurence button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return Next Occurence button.
   */
  public JButton createButtonNextOccurence(ActionListener listener, boolean icon) {
    JButton button = null;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-go-next.png", EnumImageSize.NORMAL,
          GT._("Next occurrence (Alt + &N)"), false);
    } else {
      button = Utilities.createJButton(GT._("&Next occurrence"));
    }
    button.setActionCommand(ACTION_NEXT_OCCURENCE);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Create a Previous Occurence button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return Previous Occurence button.
   */
  public JButton createButtonPreviousOccurence(ActionListener listener, boolean icon) {
    JButton button = null;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-go-previous.png", EnumImageSize.NORMAL,
          GT._("Previous occurrence (Alt + &P)"), false);
    } else {
      button = Utilities.createJButton(GT._("&Previous occurrence"));
    }
    button.setActionCommand(ACTION_PREVIOUS_OCCURENCE);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Add a component for the Redirect buttons.
   * 
   * @param panel Container.
   */
  protected void addButtonRedirect(JComponent panel) {
    buttonFullAnalysisRedirect = Utilities.createJButton(GT._(
        "Full analysis of redirect"));
    buttonFullAnalysisRedirect.setActionCommand(ACTION_FULL_ANALYSIS_REDIR);
    buttonFullAnalysisRedirect.addActionListener(this);
    panel.add(buttonFullAnalysisRedirect);
    buttonDisambiguationRedirect = Utilities.createJButton(GT._(
        "Disambiguation analysis of redirect"));
    buttonDisambiguationRedirect.setActionCommand(ACTION_DISAMBIGUATION_REDIR);
    buttonDisambiguationRedirect.addActionListener(this);
    panel.add(buttonDisambiguationRedirect);
  }

  /**
   * Add a component for the Reload button.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonReload(JComponent panel, boolean icon) {
    if (buttonReload == null) {
      buttonReload = createButtonReload(this, icon);
      panel.add(buttonReload);
    }
  }

  /**
   * Create a Reload button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return Reload button.
   */
  public JButton createButtonReload(ActionListener listener, boolean icon) {
    JButton button = null;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-view-refresh.png", EnumImageSize.NORMAL,
          GT._("Reload (Alt + &R)"), false);
    } else {
      button = Utilities.createJButton(GT._("&Reload"));
    }
    button.setActionCommand(ACTION_RELOAD);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Add a component for the Send button.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonSend(JComponent panel, boolean icon) {
    if (buttonSend == null) {
      buttonSend = createButtonSend(this, icon);
      panel.add(buttonSend);
    }
  }

  /**
   * Create a Send button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return Send button.
   */
  public JButton createButtonSend(ActionListener listener, boolean icon) {
    JButton button;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-document-send.png", EnumImageSize.NORMAL,
          GT._("Send (Alt + &S)"), false);
    } else {
      button = Utilities.createJButton(GT._("&Send"));
    }
    button.setActionCommand(ACTION_SEND);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Add a component for the Undo / Redo buttons.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonUndoRedo(JComponent panel, boolean icon) {
    if (icon) {
      buttonUndo = Utilities.createJButton(
          "gnome-edit-undo.png", EnumImageSize.NORMAL,
          GT._("Undo"), false);
    } else {
      buttonUndo = Utilities.createJButton(GT._("Undo"));
    }
    panel.add(buttonUndo);
    if (icon) {
      buttonRedo = Utilities.createJButton(
          "gnome-edit-redo.png", EnumImageSize.NORMAL,
          GT._("Redo"), false);
    } else {
      buttonRedo = Utilities.createJButton(GT._("Redo"));
    }
    panel.add(buttonRedo);
    if (textContents != null) {
      textContents.setUndoButton(buttonUndo);
      textContents.setRedoButton(buttonRedo);
    }
  }

  /**
   * Create a Validate button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return Validate button.
   */
  public JButton createButtonValidate(ActionListener listener, boolean icon) {
    JButton button;
    if (icon) {
      button = Utilities.createJButton(
          "commons-approve-icon.png", EnumImageSize.NORMAL,
          GT._("Validate (Alt + &V)"), false);
    } else {
      button = Utilities.createJButton(GT._("&Validate"));
    }
    button.setActionCommand(ACTION_VALIDATE);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Add a component for the External Viewer button.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonView(JComponent panel, boolean icon) {
    if (Utilities.isDesktopSupported() && (buttonView == null)) {
      buttonView = createButtonView(this, icon);
      panel.add(buttonView);
    }
  }

  /**
   * Create a External Viewer button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return External Viewer button.
   */
  public JButton createButtonView(ActionListener listener, boolean icon) {
    JButton button = null;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-emblem-web.png", EnumImageSize.NORMAL,
          GT._("External Viewer (Alt + &E)"), false);
    } else {
      button = Utilities.createJButton(GT._("&External Viewer"));
    }
    button.setActionCommand(ACTION_VIEW);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Add a component for the External Viewer button for the page history.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonViewHistory(JComponent panel, boolean icon) {
    if (Utilities.isDesktopSupported() && (buttonViewHistory == null)) {
      buttonViewHistory = createButtonViewHistory(this, icon);
      panel.add(buttonViewHistory);
    }
  }

  /**
   * Create a External Viewer button for the page history.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return External Viewer button.
   */
  public JButton createButtonViewHistory(ActionListener listener, boolean icon) {
    JButton button = null;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-emblem-documents.png", EnumImageSize.NORMAL,
          GT._("History (Alt + &H)"), false);
    } else {
      button = Utilities.createJButton(GT._("&History"));
    }
    button.setActionCommand(ACTION_VIEW_HISTORY);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Add a component for the Watch button.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonWatch(JComponent panel, boolean icon) {
    if (buttonWatch == null) {
      buttonWatch = createButtonWatch(this, icon);
      panel.add(buttonWatch);
    }
  }

  /**
   * Create a Watch button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return Watch button.
   */
  public JButton createButtonWatch(ActionListener listener, boolean icon) {
    JButton button;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-logviewer-add.png", EnumImageSize.NORMAL,
          GT._("Add to Watch list (Alt + &W)"), false);
    } else {
      button = Utilities.createJButton(GT._("Add to &Watch list"));
    }
    button.setActionCommand(ACTION_WATCH);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Add a component for the Automatic Comment checkbox.
   * 
   * @param panel Container.
   * @param constraints Constraints.
   */
  protected void addChkAutomaticComment(JPanel panel, GridBagConstraints constraints) {
    chkAutomaticComment = createChkAutomaticComment(true, this);
    panel.add(chkAutomaticComment, constraints);
    constraints.gridx++;
    textComment = new JTextField(getDefaultComment());
    constraints.weightx = 1;
    panel.add(textComment, constraints);
  }

  /**
   * Create a Automatic Comment checkbox.
   * 
   * @param listener Action listener.
   * @return Automatic Comment checkbox.
   */
  public JCheckBox createChkAutomaticComment(boolean checked, ItemListener listener) {
    JCheckBox checkbox = Utilities.createJCheckBox(GT._("Automatic comment"), checked);
    checkbox.addItemListener(listener);
    return checkbox;
  }

  /**
   * Add a component for the Close after sending checkbox.
   * 
   * @param panel Container.
   */
  protected void addChkCloseAfterSend(JPanel panel) {
    Configuration config = Configuration.getConfiguration();
    chkCloseAfterSend = Utilities.createJCheckBox(
        GT._("&Close after sending"),
        config.getBoolean(
            null,
            Configuration.BOOLEAN_CLOSE_FULL,
            Configuration.DEFAULT_CLOSE_FULL));
    panel.add(chkCloseAfterSend);
  }

  /**
   * Add a component for the Edit talk page checkbox.
   * 
   * @param panel Container.
   */
  protected void addChkEditTalkPage(JPanel panel) {
    if (getTextContents() != null) {
      chkEditTalkPage = Utilities.createJCheckBox(
          GT._("&Add a note on talk page"), false);
      getTextContents().setCheckBoxAddNote(chkEditTalkPage);
      panel.add(chkEditTalkPage);
    }
  }

  /**
   * Add a component for the Update disambiguation warning checkbox.
   * 
   * @param panel Container.
   */
  protected void addChkUpdateDabWarning(JPanel panel) {
    if (getTextContents() != null) {
      chkUpdateDabWarning = Utilities.createJCheckBox(
          GT._("Update disambiguation warning on talk page"),
          false);
      getTextContents().setCheckBoxUpdateDabWarning(chkUpdateDabWarning);
      panel.add(chkUpdateDabWarning);
    }
  }

  /**
   * Add a component for the Create disambiguation warning checkbox.
   * 
   * @param panel Container.
   */
  protected void addChkCreateDabWarning(JPanel panel) {
    if (getTextContents() != null) {
      chkCreateDabWarning = Utilities.createJCheckBox(
          GT._("Create disambiguation warning on talk page"),
          false);
      getTextContents().setCheckBoxUpdateDabWarning(chkCreateDabWarning);
      panel.add(chkCreateDabWarning);
    }
  }

  /**
   * Add a component for the Last modified label.
   * 
   * @param panel Container.
   */
  protected void addLblLastModified(JComponent panel) {
    lblLastModified = Utilities.createJLabel(" ");
    panel.add(lblLastModified);
  }

  /**
   * Add a component for the Edit protection level.
   * 
   * @param panel Container.
   */
  protected void addLblEditProtectionLevel(JComponent panel) {
    lblEditProtectionLevel = Utilities.createJLabel(" ");
    lblEditProtectionLevel.setToolTipText(GT._("Protection level for editing the page"));
    panel.add(lblEditProtectionLevel);
  }

  /**
   * Add a component for the Text.
   * 
   * @param panel Container.
   * @param constraints Constraints.
   */
  protected void addTextContents(JPanel panel, GridBagConstraints constraints) {
    addTextContents(panel, constraints, textContents, buttonUndo);
  }

  /**
   * Add a component for the Text.
   * 
   * @param panel Container.
   * @param constraints Constraints.
   * @param textPane Text pane.
   * @param undo Button undo.
   */
  protected void addTextContents(
      JPanel panel, GridBagConstraints constraints,
      MediaWikiPane textPane, JButton undo) {
    if (textPane != null) {
      Configuration config = Configuration.getConfiguration();
      textPane.setBackground(Color.WHITE);
      textPane.setEditable(true);
      if (undo != null) {
        textPane.setUndoLevels(config.getInt(
            null,
            Configuration.INTEGER_ANALYSIS_UNDO_LVL,
            Configuration.DEFAULT_ANALYSIS_UNDO_LVL));
      }
      textPane.addPropertyChangeListener(
          MediaWikiPane.PROPERTY_MODIFIED,
          new PropertyChangeListener() {
  
            /* (non-Javadoc)
             * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
             */
            public void propertyChange(@SuppressWarnings("unused") PropertyChangeEvent evt) {
              updateComponentState();
            }
            
          });
      JScrollPane scrollContents = new JScrollPane(textPane);
      scrollContents.setMinimumSize(new Dimension(100, 100));
      scrollContents.setPreferredSize(new Dimension(1000, 500));
      scrollContents.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      panel.add(scrollContents, constraints);
    }
  }

  /**
   * Create the text contents component.
   * 
   * @param window Window.
   */
  protected void createTextContents(BasicWindow window) {
    if (textContents == null) {
      textContents = new MediaWikiPane(getWikipedia(), page, window);
    }
  }

  /**
   * @return Text contents component.
   */
  protected MediaWikiPane getTextContents() {
    return textContents;
  }

  /**
   * Set the contents.
   */
  void setContents() {
    if (SwingUtilities.isEventDispatchThread()) {
      if (getTextContents() != null) {
        getTextContents().setWikiPage(page);
        getTextContents().setText(page.getContents());
      }
      if (lblLastModified != null) {
        if ((page.getContentsTimestamp() != null) && (!page.getContentsTimestamp().equals(""))) {
          Long duration = page.getContentsAge();
          if (duration != null) {
            if (duration.longValue() <= 5 * 60) { // 5 minutes
              lblLastModified.setForeground(Color.RED);
              lblLastModified.setToolTipText(GT._(
                  "Last modified at {0}. It was modified less than {1} minutes ago.",
                  new Object[] { page.getContentsTimestamp(), Long.valueOf(duration / 60 + 1) } ));
            } else if (duration.longValue() <= 60 * 60) { // 1 hour
              lblLastModified.setForeground(Color.ORANGE);
              lblLastModified.setToolTipText(GT._(
                  "Last modified at {0}. It was modified less than {1} minutes ago.",
                  new Object[] { page.getContentsTimestamp(), Long.valueOf(duration / 60 + 1) } ));
            } else {
              lblLastModified.setToolTipText(GT._(
                  "Last modified at {0}", page.getContentsTimestamp()));
            }
          }
          lblLastModified.setText(page.getContentsTimestamp());
          lblLastModified.setVisible(true);
        } else {
          lblLastModified.setVisible(false);
        }
      }
      if (lblEditProtectionLevel != null) {
        if ((page.getEditProtectionLevel() != null) && (!page.getEditProtectionLevel().equals(""))) {
          if ("sysop".equals(page.getEditProtectionLevel())) {
            lblEditProtectionLevel.setForeground(Color.RED);
          } else {
            lblEditProtectionLevel.setForeground(Color.ORANGE);
          }
          lblEditProtectionLevel.setText(" (" + page.getEditProtectionLevel() + ")");
        } else {
          lblEditProtectionLevel.setVisible(false);
        }
      }
      updateComponentState();
    } else {
      SwingUtilities.invokeLater(new Runnable() {
        public void run() {
          setContents();
        }
      });
    }
  }

  /* ====================================================================== */
  /* Menus                                                                  */
  /* ====================================================================== */

  private JMenu menuFixRedirects;

  /**
   * @return Fix redirects menu.
   */
  protected JMenu createFixRedirectsMenu() {
    if (menuFixRedirects == null) {
      menuFixRedirects = Utilities.createJMenu(GT._("Fix &redirects"));
    } else {
      menuFixRedirects.removeAll();
    }
    if ((page != null) && (page.getLinks() != null)) {
      for (Page p : page.getLinks()) {
        if (p.isRedirect() && !Boolean.TRUE.equals(p.isDisambiguationPage())) {
          String newTitle = p.getRedirectDestination();
          String text = GT._(
              "Link \"{0}\" to \"{1}\"", new Object[] { p.getTitle(), newTitle });
          JMenuItem menuItem = new JMenuItem(text);
          menuItem.addActionListener(new ReplaceAllLinksAction(getTextContents(), p, newTitle));
          menuFixRedirects.add(menuItem);
        }
      }
    }
    return menuFixRedirects;
  }

  /**
   * @return Tools menu.
   */
  protected JMenu createToolsMenu() {
    JMenu menu = Utilities.createJMenu(GT._("&Tools"));
    JMenuItem menuItem = null;
    menu.add(createFixRedirectsMenu());

    menuItem = Utilities.createJMenuItem(GT._("&Preview"));
    menuItem.setActionCommand(ACTION_PREVIEW);
    menuItem.addActionListener(this);
    menu.add(menuItem);

    menuItem = Utilities.createJMenuItem(GT._("&Expand templates"));
    menuItem.setActionCommand(ACTION_EXPAND_TEMPLATES);
    menuItem.addActionListener(this);
    menu.add(menuItem);

    menuItem = Utilities.createJMenuItem(GT._("Expand templates &and Preview"));
    menuItem.setActionCommand(ACTION_EXPAND_PREVIEW);
    menuItem.addActionListener(this);
    menu.add(menuItem);
 
    return menu;
  }

  /* ====================================================================== */
  /* ItemListener                                                           */
  /* ====================================================================== */

  /* (non-Javadoc)
   * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
   */
  public void itemStateChanged(ItemEvent e) {
    if ((e == null) || (e.getSource() == null)) {
      return;
    }
    Object source = e.getSource();
    if ((chkAutomaticComment != null) &&
        (textComment != null) &&
        (source == chkAutomaticComment)) {
      textComment.setEnabled(!chkAutomaticComment.isSelected());
      if (chkAutomaticComment.isSelected()) {
        textComment.setText(getDefaultComment());
      }
    }
  }

  /* ====================================================================== */
  /* ActionListener                                                         */
  /* ====================================================================== */

  public final static String ACTION_DISAMBIGUATION_PAGE  = "DISAMBIGUATION PAGE";
  public final static String ACTION_DISAMBIGUATION_REDIR = "DISAMBIGUATION REDIR";
  public final static String ACTION_EXPAND_TEMPLATES     = "EXPAND TEMPLATES";
  public final static String ACTION_EXPAND_PREVIEW       = "EXPAND PREVIEW";
  public final static String ACTION_FIRST_OCCURENCE      = "FIRST OCCURENCE";
  public final static String ACTION_FULL_ANALYSIS_PAGE   = "FULL ANALYSIS PAGE";
  public final static String ACTION_FULL_ANALYSIS_REDIR  = "FULL ANALYSIS REDIR";
  public final static String ACTION_LAST_OCCURENCE       = "LAST OCCURENCE";
  public final static String ACTION_NEXT_OCCURENCE       = "NEXT OCCURENCE";
  public final static String ACTION_PREVIEW              = "PREVIEW";
  public final static String ACTION_PREVIOUS_OCCURENCE   = "PREVIOUS OCCURENCE";
  public final static String ACTION_RELOAD               = "RELOAD";
  public final static String ACTION_SEND                 = "SEND";
  public final static String ACTION_VALIDATE             = "VALIDATE";
  public final static String ACTION_VIEW                 = "VIEW";
  public final static String ACTION_VIEW_HISTORY         = "VIEW_HISTORY";
  public final static String ACTION_WATCH                = "WATCH";

  /**
   * Invoked when an action occurs.
   * 
   * @param e Event.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    if (ACTION_DISAMBIGUATION_PAGE.equals(e.getActionCommand())) {
      actionDisambiguation();
    } else if (ACTION_DISAMBIGUATION_REDIR.equals(e.getActionCommand())) {
      actionDisambiguationRedir();
    } else if (ACTION_EXPAND_TEMPLATES.equals(e.getActionCommand())) {
      actionExpandTemplates();
    } else if (ACTION_EXPAND_PREVIEW.equals(e.getActionCommand())) {
      actionExpandTemplatesPreview();
    } else if (ACTION_FULL_ANALYSIS_PAGE.equals(e.getActionCommand())) {
      actionFullAnalysis();
    } else if (ACTION_FULL_ANALYSIS_REDIR.equals(e.getActionCommand())) {
      actionFullAnalysisRedir();
    } else if (ACTION_PREVIEW.equals(e.getActionCommand())) {
      actionPreview();
    } else if (ACTION_RELOAD.equals(e.getActionCommand())) {
      actionReload();
    } else if (ACTION_SEND.equals(e.getActionCommand())) {
      actionSend();
    } else if (ACTION_VALIDATE.equals(e.getActionCommand())) {
      actionValidate();
    } else if (ACTION_VIEW.equals(e.getActionCommand())) {
      actionView();
    } else if (ACTION_VIEW_HISTORY.equals(e.getActionCommand())) {
      actionViewHistory();
    } else if (ACTION_WATCH.equals(e.getActionCommand())) {
      actionWatch();
    }
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  private void actionDisambiguation() {
    Controller.runDisambiguationAnalysis(getPageName(), getWikipedia());
  }

  /**
   * Action called when Disambiguation Redirect button is pressed.
   */
  private void actionDisambiguationRedir() {
    if (page != null) {
      Controller.runDisambiguationAnalysis(
          page.getRedirectTitle(), getWikipedia());
    }
  }

  /**
   * Action called when Expand Templates menu is selected. 
   */
  private void actionExpandTemplates() {
    if (textContents != null) {
      Controller.runExpandTemplates(
          getPageName(), textContents.getText(),
          true, false, getWikipedia());
    }
  }

  /**
   * Action called when Expand Templates / Preview menu is selected. 
   */
  private void actionExpandTemplatesPreview() {
    if (textContents != null) {
      Controller.runExpandTemplates(
          getPageName(), textContents.getText(),
          true, true, getWikipedia());
    }
  }

  /**
   * Action called when Full analysis button is pressed.
   */
  private void actionFullAnalysis() {
    Controller.runFullAnalysis(getPageName(), null, getWikipedia());
  }

  /**
   * Action called when Full analysis Redirect button is pressed.
   */
  private void actionFullAnalysisRedir() {
    if (page != null) {
      Controller.runFullAnalysis(
          page.getRedirectTitle(), null,
          getWikipedia());
    }
  }

  /**
   * Action called when Preview menu is selected. 
   */
  private void actionPreview() {
    if (textContents != null) {
      Controller.runExpandTemplates(
          getPageName(), textContents.getText(),
          false, true, getWikipedia());
    }
  }

  /**
   * Action called when Reload button is pressed.
   */
  protected abstract void actionReload();

  /**
   * Action called when Send button is pressed.
   */
  private void actionSend() {
    actionValidate();

    // Check that a comment is available
    if ((textComment != null) &&
        (textComment.getText().trim().length() == 0)) {
      Utilities.displayWarning(getParentComponent(), GT._(
          "A comment is required for sending the page."));
      return;
    }

    if ((chkEditTalkPage != null) && chkEditTalkPage.isSelected() &&
        (page != null) && (page.getTalkPage(getWikipedia().getNamespaces()) != null)) {
      Controller.runNewSection(
          page.getTalkPage(getWikipedia().getNamespaces()),
          getTextContents().getText(),
          getWikipedia());
    }
    Configuration config = Configuration.getConfiguration();
    final boolean hideWindow = config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_HIDE_SENDING,
        Configuration.DEFAULT_ANALYSIS_HIDE_SENDING);
    final boolean forceWatch = config.getBoolean(
        null,
        Configuration.BOOLEAN_FORCE_WATCH,
        Configuration.DEFAULT_FORCE_WATCH);
    final int oldState = getParentComponent().getExtendedState();
    final boolean updateDabWarning = (chkUpdateDabWarning != null) && (chkUpdateDabWarning.isSelected());
    final boolean createDabWarning = (chkCreateDabWarning != null) && (chkCreateDabWarning.isSelected());
    if (hideWindow) {
      getParentComponent().setExtendedState(Frame.ICONIFIED);
    }
    getParentComponent().setTitle(GT._("Sending {0}", page.getTitle()));
    SendWorker sendWorker = new SendWorker(
        getWikipedia(), this, page, getTextContents().getText(),
        (textComment != null) ? textComment.getText() : getWikipedia().getUpdatePageMessage(),
        forceWatch, updateDabWarning, createDabWarning,
        computeErrorsFixed());
    sendWorker.setListener(new DefaultBasicWorkerListener() {
      @Override
      public void afterFinished(
          BasicWorker worker,
          boolean ok) {
        if (!worker.shouldContinue()) {
          return;
        }
        if (ok) {
          if ((chkCloseAfterSend != null) && chkCloseAfterSend.isSelected()) {
            dispose();
          } else {
            worker.getWindow().setWindowTitle(getTitle());
            worker.getWindow().setExtendedState(oldState);
            actionReload();
          }
        } else {
          worker.getWindow().setWindowTitle(getTitle());
          worker.getWindow().setExtendedState(oldState);
        }
      }
    });
    sendWorker.start();
  }

  /**
   * Action called when Validate button is pressed. 
   */
  protected void actionValidate() {
    // Implement in sub-classes
  }

  /**
   * Action called when View button is pressed. 
   */
  private void actionView() {
    Utilities.browseURL(getWikipedia(), getPageName(), false);
  }

  /**
   * Action called when View History button is pressed. 
   */
  private void actionViewHistory() {
    Utilities.browseURL(getWikipedia(), getPageName(), "history");
  }

  /**
   * Action called when Watch button is pressed. 
   */
  private void actionWatch() {
    String message = GT._("Would you like to add this page on your local Watch list ?"); 
    if (displayYesNoWarning(message) == JOptionPane.YES_OPTION) {
      Configuration config = Configuration.getConfiguration();
      List<String> watch = config.getStringList(
          getWikipedia(), Configuration.ARRAY_WATCH_PAGES);
      if (!watch.contains(page.getTitle())) {
        watch.add(page.getTitle());
        Collections.sort(watch);
        config.setStringList(getWikipedia(), Configuration.ARRAY_WATCH_PAGES, watch);
      }
    }
  }

  // =========================================================================
  // Check Wiki
  // =========================================================================
  
  private List<CheckErrorPage> initialErrors;

  protected void initializeInitialErrors(
      Collection<CheckErrorAlgorithm> algorithms) {
    if (page != null) {
      List<CheckErrorPage> errorsFound = CheckError.analyzeErrors(
          algorithms, page, page.getContents());
      initialErrors = new ArrayList<CheckErrorPage>();
      if (errorsFound != null) {
        for (CheckErrorPage tmpError : errorsFound) {
          initialErrors.add(tmpError);
        }
      }
    }
  }

  /**
   * @return Initial errors.
   */
  protected Collection<CheckErrorPage> getInitialErrors() {
    return initialErrors;
  }

  /**
   * Mark a page as fixed for an error.
   * 
   * @param error Error.
   * @param errorNumber Error number.
   * @param pageFixed Page.
   */
  static public void markPageAsFixed(
      final CheckError error, final String errorNumber, final Page pageFixed) {
    if ((pageFixed != null) && (pageFixed.getPageId() != null)) {
      MediaWikiController.addSimpleTask(new Callable<Page>() {
  
        public Page call() throws Exception
        {
          if (error != null) {
            error.fix(pageFixed);
          } else {
            CheckError.fix(pageFixed, errorNumber);
          }
          return pageFixed;
        }});
    }
  }

  /**
   * @return Errors fixed.
   */
  protected List<CheckErrorAlgorithm> computeErrorsFixed() {
    final List<CheckErrorAlgorithm> errorsFixed = new ArrayList<CheckErrorAlgorithm>();
    if (initialErrors != null) {
      for (CheckErrorPage initialError : initialErrors) {
        CheckError.analyzeError(initialError, getTextContents().getText());
        if (initialError.getErrorFound() == false) {
          errorsFixed.add(initialError.getAlgorithm());
        }
      }
    }
    return errorsFixed;
  }
}
