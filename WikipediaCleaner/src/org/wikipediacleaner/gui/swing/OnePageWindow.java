/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
import java.beans.EventHandler;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWikiController;
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageRedirect;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionFullAnalysis;
import org.wikipediacleaner.gui.swing.action.ReplaceAllLinksAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.gui.swing.worker.SendWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;

/**
 * A base class for Wikipedia Cleaner windows with one page contents.
 */
public abstract class OnePageWindow
  extends PageWindow {

  private Page page;
  private String pageName;

  private boolean pageLoaded = false;

  /**
   * @return Page.
   */
  @Override
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
   * @param pageAnalysis Page analysis.
   * @return Default comment.
   */
  protected String getAutomaticComment(PageAnalysis pageAnalysis) {
    return getWikipedia().getConfiguration().getUpdatePageMessage();
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
   * @return Comment
   */
  protected String getComment() {
    if (textComment == null) {
      return null;
    }
    return textComment.getText();
  }

  /**
   * Clean page. 
   */
  protected void clean() {
    pageLoaded = false;
    if (getTextContents() != null) {
      getTextContents().setText(null);
    }
    page = DataManager.createSimplePage(getWikipedia(), getTextPageName(), null, null, null);
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
  private JButton buttonAnalysisRedirect;
  private JButton buttonRedo;
  private JButton buttonUndo;
  private JButton buttonReload;
  private JButton buttonSend;
  private JButton buttonFullAnalysis;
  private JTextField textComment;
  private JCheckBox chkAutomaticComment;
  private JToggleButton chkSpelling;
  private JLabel lblLastModified;
  private JLabel lblEditProtectionLevel;

  private JButton buttonOptions;
  private JPopupMenu menuOptions;
  JMenuItem chkCloseAfterSend;
  private JMenuItem chkEditTalkPage;
  private JMenuItem chkMarkEditMinor;
  private JMenuItem chkRemoveBotFlag;
  JMenuItem chkUpdateDabWarning;
  JMenuItem chkCreateDabWarning;

  private MWPane textContents;

  /**
   * Update component state.
   */
  @Override
  protected void updateComponentState() {
    boolean redirect = (page != null) && (page.getRedirects().isRedirect());
    boolean article = (page != null) && (page.isArticle());
    boolean dabWarning =
        article &&
        (getConfiguration().getString(WPCConfigurationString.DAB_WARNING_TEMPLATE) != null) &&
        (getConfiguration().getStringList(WPCConfigurationStringList.TODO_TEMPLATES) != null);

    setEnabledStatus(textComment, (chkAutomaticComment == null) || (!chkAutomaticComment.isSelected()));
    setEnabledStatus(textContents, pageLoaded);

    setEnabledStatus(buttonDisambiguation, pageLoaded);
    setEnabledStatus(buttonFullAnalysis, pageLoaded);
    setEnabledStatus(buttonAnalysisRedirect, redirect);
    setVisibleStatus(buttonAnalysisRedirect, redirect);
    setEnabledStatus(buttonOptions, pageLoaded);
    setEnabledStatus(buttonSend, pageLoaded && (textContents != null) && textContents.isModified());

    setEnabledStatus(chkCloseAfterSend, pageLoaded);
    setEnabledStatus(chkCreateDabWarning,
        pageLoaded && dabWarning &&
        (chkUpdateDabWarning != null) &&
        (chkUpdateDabWarning.isSelected()));
    setEnabledStatus(chkEditTalkPage, pageLoaded && article);
    setEnabledStatus(chkMarkEditMinor, pageLoaded);
    setEnabledStatus(chkRemoveBotFlag, pageLoaded);
    setEnabledStatus(chkSpelling, pageLoaded);
    setEnabledStatus(chkUpdateDabWarning, pageLoaded && dabWarning);

    setEnabledStatus(menuFixRedirects, (menuFixRedirects != null) && (menuFixRedirects.getItemCount() > 0));
  }

  /**
   * Add a component for the page name.
   * 
   * @param panel Container.
   */
  protected void addTextPageName(JPanel panel) {
    if (textPagename == null) {
      textPagename = new JLabel(getPageName());
      JLabel labelPagename = Utilities.createJLabel(GT._T("&Page :"));
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
            GT._T("Disambiguation"), false, null);
      } else {
        buttonDisambiguation = Utilities.createJButton(GT._T("Disambiguation"), null);
      }
      buttonDisambiguation.addActionListener(EventHandler.create(
          ActionListener.class, this, "actionDisambiguation"));
      panel.add(buttonDisambiguation);
    }
  }

  /**
   * Add a component for the Full Analysis button.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonFullAnalysis(JComponent panel, boolean icon) {
    if (buttonFullAnalysis == null) {
      buttonFullAnalysis = ActionFullAnalysis.createButton(
          getWikipedia(), getPageName(), true, false, true);
      panel.add(buttonFullAnalysis);
    }
  }

  /**
   * Add a component for the Redirect buttons.
   * 
   * @param panel Container.
   */
  protected void addButtonRedirect(JComponent panel) {
    buttonAnalysisRedirect = Utilities.createJButton(
        "commons-redirect-arrow-without-text.png", EnumImageSize.NORMAL,
        GT._T("Redirect"), false, null);
    buttonAnalysisRedirect.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAnalysisRedir"));
    panel.add(buttonAnalysisRedirect);
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
          GT._T("Reload (Alt + &R)"), false, null);
    } else {
      button = Utilities.createJButton(GT._T("&Reload"), null);
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
    JButton button = Utilities.createJButton(
        icon ? "gnome-document-send.png" : null,
        EnumImageSize.NORMAL,
        GT._T("Send"), !icon,
        ConfigurationValueShortcut.SEND);
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
          GT._T("Undo"), false, null);
    } else {
      buttonUndo = Utilities.createJButton(GT._T("Undo"), null);
    }
    panel.add(buttonUndo);
    if (icon) {
      buttonRedo = Utilities.createJButton(
          "gnome-edit-redo.png", EnumImageSize.NORMAL,
          GT._T("Redo"), false, null);
    } else {
      buttonRedo = Utilities.createJButton(GT._T("Redo"), null);
    }
    panel.add(buttonRedo);
    if (textContents != null) {
      textContents.getUndoManager().setUndoButton(buttonUndo);
      textContents.getUndoManager().setRedoButton(buttonRedo);
    }
  }

  /**
   * Create a TOC button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return TOC button.
   */
  public JButton createButtonToc(ActionListener listener, boolean icon) {
    JButton button;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-format-indent-more.png", EnumImageSize.NORMAL,
          GT._T("Table of Contents"), false, null);
    } else {
      button = Utilities.createJButton(GT._T("Table of Contents"), null);
    }
    button.setActionCommand(ACTION_TOC);
    button.addActionListener(listener);
    return button;
  }

  /**
   * Create a Validate button.
   * 
   * @param listener Action listener.
   * @param icon Flag indicating if an icon should be used.
   * @return Validate button.
   */
  public JButton createButtonValidate(ActionListener listener, boolean icon) {
    JButton button = Utilities.createJButton(
        icon ? "commons-approve-icon.png" : null,
        EnumImageSize.NORMAL,
        GT._T("Validate"), !icon,
        ConfigurationValueShortcut.VALIDATE);
    button.setActionCommand(ACTION_VALIDATE);
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
    textComment = new JTextField(getAutomaticComment(null));
    constraints.weightx = 1;
    panel.add(textComment, constraints);
  }

  /**
   * Add a component for the Manual Comment.
   * 
   * @param panel Container.
   * @param constraints constraints.
   */
  protected void addComment(JPanel panel, GridBagConstraints constraints) {
    JLabel label = Utilities.createJLabel(GT._T("Comment"));
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 1;
    constraints.weightx = 0;
    panel.add(label, constraints);
    constraints.gridx++;
    textComment = new JTextField("");
    constraints.weightx = 1;
    panel.add(textComment, constraints);
  }

  /**
   * Create a Automatic Comment checkbox.
   *
   * @param checked True if the checkbox should be checked.
   * @param listener Action listener.
   * @return Automatic Comment checkbox.
   */
  public JCheckBox createChkAutomaticComment(boolean checked, ItemListener listener) {
    JCheckBox checkbox = Utilities.createJCheckBox(GT._T("Automatic comment"), checked);
    checkbox.addItemListener(listener);
    return checkbox;
  }

  /**
   * Add a component for the Options button.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  protected void addButtonOptions(JComponent panel, boolean icon) {
    if (icon) {
      buttonOptions = Utilities.createJButton(
          "gnome-preferences-other.png", EnumImageSize.NORMAL,
          GT._T("Options"), false, null);
    } else {
      buttonOptions = Utilities.createJButton(GT._T("Options"), null);
    }
    buttonOptions.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOptions"));
    panel.add(buttonOptions);

    Configuration config = Configuration.getConfiguration();
    menuOptions = new JPopupMenu();
    chkMarkEditMinor = Utilities.createJCheckBoxMenuItm(
        GT._T("Mark edit as minor"),
        config.getBoolean(
            null,
            ConfigurationValueBoolean.MARK_EDIT_MINOR));
    menuOptions.add(chkMarkEditMinor);
    chkRemoveBotFlag = Utilities.createJCheckBoxMenuItm(
        GT._T("Disable bot flag for this edit"),
        false);
    menuOptions.add(chkRemoveBotFlag);
    chkCloseAfterSend = Utilities.createJCheckBoxMenuItm(
        GT._T("&Close after sending"),
        config.getBoolean(
            null,
            ConfigurationValueBoolean.CLOSE_FULL));
    menuOptions.add(chkCloseAfterSend);
    chkEditTalkPage = Utilities.createJCheckBoxMenuItm(
        GT._T("&Add a note on talk page"), false);
    getTextContents().setCheckBoxAddNote(chkEditTalkPage);
    menuOptions.add(chkEditTalkPage);
    chkUpdateDabWarning = Utilities.createJCheckBoxMenuItm(
        GT._T("Update disambiguation warning on talk page"),
        false);
    getTextContents().setCheckBoxUpdateDabWarning(chkUpdateDabWarning);
    menuOptions.add(chkUpdateDabWarning);
    chkCreateDabWarning = Utilities.createJCheckBoxMenuItm(
        GT._T("Create disambiguation warning on talk page"),
        false);
    getTextContents().setCheckBoxCreateDabWarning(chkCreateDabWarning);
    menuOptions.add(chkCreateDabWarning);
  }

  /**
   * Action called when Check Spelling button is pressed.
   */
  public void actionOptions() {
    menuOptions.show(buttonOptions, 0, buttonOptions.getHeight());
  }

  /**
   * Add a button for checking spelling.
   * 
   * @param panel Container.
   * @param icon Flag indicating if an icon should be used.
   */
  public void addChkSpelling(JComponent panel, boolean icon) {
    boolean checked = shouldCheckSpelling();
    if (icon) {
      chkSpelling = Utilities.createJToggleButton(
          "gnome-tools-check-spelling.png", EnumImageSize.NORMAL,
          GT._T("Check spelling and typography"), false);
    } else {
      chkSpelling = Utilities.createJToggleButton(GT._T("Spelling"));
    }
    chkSpelling.setSelected(checked);
    panel.add(chkSpelling);
  }

  /**
   * @return True if spelling should be checked.
   */
  public boolean shouldCheckSpelling() {
    if (chkSpelling != null) {
      return chkSpelling.isSelected();
    }
    Configuration config = Configuration.getConfiguration();
    return config.getBoolean(
        null, ConfigurationValueBoolean.SPELLING);
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
    lblEditProtectionLevel.setToolTipText(GT._T("Protection level for editing the page"));
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
      MWPane textPane, JButton undo) {
    if (textPane != null) {
      Configuration config = Configuration.getConfiguration();
      textPane.setBackground(Color.WHITE);
      textPane.setEditable(true);
      if (undo != null) {
        textPane.getUndoManager().setUndoLevels(config.getInt(
            null,
            ConfigurationValueInteger.ANALYSIS_UNDO_LVL));
      }
      textPane.addPropertyChangeListener(
          MWPane.PROPERTY_MODIFIED,
          new PropertyChangeListener() {
  
            /* (non-Javadoc)
             * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
             */
            @Override
            public void propertyChange(@SuppressWarnings("unused") PropertyChangeEvent evt) {
              updateComponentState();
            }
            
          });
      JComponent scrollContents = MWPane.createComplexPane(textPane);
      scrollContents.setMinimumSize(new Dimension(100, 100));
      scrollContents.setPreferredSize(new Dimension(1000, 500));
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
      textContents = new MWPane(getWikipedia(), page, window);
      textContents.addPropertyChangeListener(MWPane.PROPERTY_MODIFIED, this);
    }
  }

  /**
   * @return Text contents component.
   */
  protected MWPane getTextContents() {
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
            long minutes = duration.longValue() / 60 + 1;
            if (minutes > 60) {
              lblLastModified.setToolTipText(GT._T(
                  "Last modified at {0}", page.getContentsTimestamp()));
            } else {
              if (minutes <= 5) {
                lblLastModified.setForeground(Color.RED);
              } else {
                lblLastModified.setForeground(Color.ORANGE);
              }
              lblLastModified.setToolTipText(GT.__(
                  "Last modified at {0}. It was modified less than {1} minute ago.",
                  "Last modified at {0}. It was modified less than {1} minutes ago.",
                  minutes,
                  new Object[] { page.getContentsTimestamp(), Long.valueOf(minutes) } ));
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
        @Override
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
      menuFixRedirects = Utilities.createJMenu(GT._T("Fix &redirects"));
    } else {
      menuFixRedirects.removeAll();
    }
    if ((page != null) && (page.getLinks() != null)) {
      for (Page p : page.getLinks()) {
        PageRedirect redirects = p.getRedirects();
        if (redirects.isRedirect() && !Boolean.TRUE.equals(p.isDisambiguationPage())) {
          String newTitle = redirects.getDestination();
          String text = GT._T(
              "Link \"{0}\" to \"{1}\"", new Object[] { p.getTitle(), newTitle });
          JMenuItem menuItem = new JMenuItem(text);
          String warning = getConfiguration().getString(
              WPCConfigurationString.REDIRECT_WARNING_BEFORE_REPLACEMENT);
          menuItem.addActionListener(new ReplaceAllLinksAction(
              getTextContents(), p, newTitle, warning));
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
    JMenu menu = Utilities.createJMenu(GT._T("&Tools"));
    JMenuItem menuItem = null;
    menu.add(createFixRedirectsMenu());

    menuItem = Utilities.createJMenuItem(GT._T("&Preview"), false);
    menuItem.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionPreview"));
    menu.add(menuItem);

    menuItem = Utilities.createJMenuItem(GT._T("&Expand templates"), false);
    menuItem.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionExpandTemplates"));
    menu.add(menuItem);

    menuItem = Utilities.createJMenuItem(GT._T("Expand templates &and Preview"), false);
    menuItem.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionExpandTemplatesPreview"));
    menu.add(menuItem);
 
    return menu;
  }

  /* ====================================================================== */
  /* ItemListener                                                           */
  /* ====================================================================== */

  /* (non-Javadoc)
   * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
   */
  @Override
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
        textComment.setText(getAutomaticComment(null));
      }
    }
  }

  /* ====================================================================== */
  /* PropertyChange                                                         */
  /* ====================================================================== */

  /* (non-Javadoc)
   * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
   */
  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    if (evt == null) {
      return;
    }
    if (MWPane.PROPERTY_MODIFIED.equals(evt.getPropertyName())) {
      updateComponentState();
    }
  }

  /* ====================================================================== */
  /* ActionListener                                                         */
  /* ====================================================================== */

  public final static String ACTION_FULL_ANALYSIS_PAGE   = "FULL ANALYSIS PAGE";
  public final static String ACTION_RELOAD               = "RELOAD";
  public final static String ACTION_SEND                 = "SEND";
  public final static String ACTION_TOC                  = "TOC";
  public final static String ACTION_VALIDATE             = "VALIDATE";

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

    if (ACTION_FULL_ANALYSIS_PAGE.equals(e.getActionCommand())) {
      actionFullAnalysis();
    } else if (ACTION_RELOAD.equals(e.getActionCommand())) {
      actionReload();
    } else if (ACTION_SEND.equals(e.getActionCommand())) {
      actionSend();
    } else if (ACTION_TOC.equals(e.getActionCommand())) {
      actionToc();
    } else if (ACTION_VALIDATE.equals(e.getActionCommand())) {
      actionValidate(true);
    }
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  public void actionDisambiguation() {
    Controller.runDisambiguationAnalysis(getPageName(), getWikipedia());
  }

  /**
   * Action called when Analysis Redirect button is pressed.
   */
  public void actionAnalysisRedir() {
    JPopupMenu menu = new JPopupMenu();
    JMenuItem item = new JMenuItem(
        GT._T("Full analysis"),
        Utilities.getImageIcon("gnome-system-run.png", EnumImageSize.NORMAL));
    item.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionFullAnalysisRedir"));
    menu.add(item);
    item = new JMenuItem(
        GT._T("Disambiguation analysis"),
        Utilities.getImageIcon("commons-disambig-colour.png", EnumImageSize.NORMAL));
    item.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDisambiguationRedir"));
    menu.add(item);
    menu.show(
        buttonAnalysisRedirect,
        0,
        buttonAnalysisRedirect.getHeight());
  }

  /**
   * Action called when Disambiguation Redirect button is pressed.
   */
  public void actionDisambiguationRedir() {
    if (page != null) {
      Controller.runDisambiguationAnalysis(
          page.getRedirects().getTitle(), getWikipedia());
    }
  }

  /**
   * Action called when Expand Templates menu is selected. 
   */
  public void actionExpandTemplates() {
    if (textContents != null) {
      Controller.runExpandTemplates(
          getPageName(), textContents.getText(),
          true, false, getWikipedia());
    }
  }

  /**
   * Action called when Expand Templates / Preview menu is selected. 
   */
  public void actionExpandTemplatesPreview() {
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
  public void actionFullAnalysisRedir() {
    if (page != null) {
      Controller.runFullAnalysis(
          page.getRedirects().getTitle(), null,
          getWikipedia());
    }
  }

  /**
   * Action called when Preview menu is selected. 
   */
  public void actionPreview() {
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
    actionValidate(false);

    // Check that a comment is available
    if ((textComment != null) &&
        (textComment.getText().trim().length() == 0)) {
      Utilities.displayWarning(getParentComponent(), GT._T(
          "A comment is required for sending the page."));
      return;
    }

    if ((chkEditTalkPage != null) && chkEditTalkPage.isSelected() &&
        (page != null) &&
        (page.getTalkPage() != null)) {
      Controller.runNewSection(
          page.getTalkPage(),
          getTextContents().getText(), page.getTitle(),
          getWikipedia());
    }
    Configuration config = Configuration.getConfiguration();
    final boolean hideWindow = config.getBoolean(
        null,
        ConfigurationValueBoolean.ANALYSIS_HIDE_SENDING);
    final boolean forceWatch = config.getBoolean(
        null,
        ConfigurationValueBoolean.FORCE_WATCH);
    final boolean minor = (chkMarkEditMinor == null) || (chkMarkEditMinor.isSelected());
    final boolean bot = (chkRemoveBotFlag == null) || (!chkRemoveBotFlag.isSelected());
    final int oldState = getParentComponent().getExtendedState();
    final boolean updateDabWarning = (chkUpdateDabWarning != null) && (chkUpdateDabWarning.isSelected());
    final boolean createDabWarning = (chkCreateDabWarning != null) && (chkCreateDabWarning.isSelected());
    final boolean createISBNWarning = false;
    boolean updateISBNWarning = false;
    final boolean createISSNWarning = false;
    boolean updateISSNWarning = false;
    final boolean createDuplicateArgsWarning = false;
    boolean updateDuplicateArgsWarning = false;
    List<AlgorithmError.Progress> errorsFixed = computeErrorsFixed();
    if (errorsFixed != null) {
      for (AlgorithmError.Progress errorFixed : errorsFixed) {
        CheckErrorAlgorithm algorithm = errorFixed.algorithm;
        int errorNumber = algorithm.getErrorNumber();
        if ((errorNumber == 69) ||
            (errorNumber == 70) ||
            (errorNumber == 71) ||
            (errorNumber == 72) ||
            (errorNumber == 73)) {
          updateISBNWarning = true;
        }
        if ((errorNumber == 106) ||
            (errorNumber == 107) ||
            (errorNumber == 108)) {
          updateISSNWarning = true;
        }
        if (errorNumber == 524) {
          updateDuplicateArgsWarning = true;
        }
      }
    }
    if (hideWindow) {
      getParentComponent().setExtendedState(Frame.ICONIFIED);
    }
    getParentComponent().setTitle(GT._T("Sending {0}", page.getTitle()));
    SendWorker sendWorker = new SendWorker.Builder().
        allowDabWarning(updateDabWarning, createDabWarning).
        allowISBNWarning(updateISBNWarning, createISBNWarning).
        allowISSNWarning(updateISSNWarning, createISSNWarning).
        allowDuplicateArgsWarning(updateDuplicateArgsWarning, createDuplicateArgsWarning).
        createWorker(
          getWikipedia(), this, page, getTextContents().getText(),
          (textComment != null) ?
              textComment.getText() :
              getWikipedia().getConfiguration().getUpdatePageMessage(),
          bot, minor, forceWatch,
          getContributions(), errorsFixed);
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
   * Action called when TOC button is pressed. 
   */
  private void actionToc() {
    if (getTextContents() != null) {
      getTextContents().toggleToc();
    }
  }

  /**
   * Action called when Validate button is pressed.
   * 
   * @param fullValidate Full validation ?
   */
  protected void actionValidate(boolean fullValidate) {
    // Implement in sub-classes
  }

  // =========================================================================
  // Check Wiki
  // =========================================================================
  
  private List<CheckErrorPage> initialErrors;

  /**
   * Initialize list of initial errors.
   * 
   * @param algorithms Algorithms.
   */
  protected void initializeInitialErrors(
      Collection<CheckErrorAlgorithm> algorithms) {
    if (page != null) {
      PageAnalysis pageAnalysis = page.getAnalysis(page.getContents(), false);
      pageAnalysis.shouldCheckSpelling(shouldCheckSpelling());
      List<CheckErrorPage> errorsFound = AlgorithmError.analyzeErrors(
          algorithms, pageAnalysis, false);
      initialErrors = new ArrayList<>();
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
   * @return Initial algorithms.
   */
  protected Collection<CheckErrorAlgorithm> getInitialAlgorithms() {
    if (initialErrors == null) {
      return null;
    }
    Collection<CheckErrorAlgorithm> algorithms = new ArrayList<>();
    for (CheckErrorPage error : initialErrors) {
      CheckErrorAlgorithm algorithm = error.getAlgorithm();
      if (!algorithms.contains(algorithm)) {
        algorithms.add(algorithm);
      }
    }
    return algorithms;
  }

  /**
   * Mark a page as fixed for an error.
   * 
   * @param errorNumber Error number.
   * @param pageFixed Page.
   */
  static public void markPageAsFixed(
      final String errorNumber, final Page pageFixed) {
    if (pageFixed != null) {
      MediaWikiController.addSimpleTask(new Callable<Page>() {
  
        @Override
        public Page call() throws Exception
        {
          CheckWiki checkWiki = APIFactory.getCheckWiki();
          if (checkWiki != null) {
            checkWiki.markAsFixed(pageFixed, errorNumber);
          }
          return pageFixed;
        }});
    }
  }

  /**
   * @return Errors fixed.
   */
  protected List<AlgorithmError.Progress> computeErrorsFixed() {
    return AlgorithmError.computeErrorsFixed(initialErrors, getTextContents().getText(), shouldCheckSpelling());
  }
}
