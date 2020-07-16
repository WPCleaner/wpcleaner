/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.checkwiki;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.EventHandler;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.CheckWikiDetection;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.Contributions;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AutomaticFormatter;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.dataaccess.PageProvider;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.OnePageWindow;
import org.wikipediacleaner.gui.swing.action.ActionDeletePage;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.gui.swing.action.ActionFullAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionInsertPredefinedText;
import org.wikipediacleaner.gui.swing.action.ActionOccurrence;
import org.wikipediacleaner.gui.swing.action.ListenerPageDeletion;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.CheckErrorPageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.CheckErrorPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.gui.swing.component.MWPaneBasicFormatter;
import org.wikipediacleaner.gui.swing.component.MWPaneCheckWikiFormatter;
import org.wikipediacleaner.gui.swing.component.MWPaneCheckWikiPopupListener;
import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;
import org.wikipediacleaner.gui.swing.deadlink.ActionDeadLink;
import org.wikipediacleaner.gui.swing.linter.ActionLinter;
import org.wikipediacleaner.gui.swing.worker.SendWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueString;

/**
 * Component for working on a page in the CheckWiki project.
 */
public class CheckWikiContentPanel
  extends JPanel
  implements ActionListener, ItemListener, ListenerPageDeletion, PageProvider {

  private static final long serialVersionUID = 1L;

  public final static String ACTION_MARK_AS_FIXED = "MARK_AS_FIXED";

  final CheckWikiWindow window;
  final JTabbedPane pane;
  final Page page;
  private final List<AlgorithmError> errors;

  private JList<CheckErrorPage> listErrors;
  private DefaultListModel<CheckErrorPage> modelErrors;
  private List<CheckErrorPage> initialErrors;
  private JTextField textComment;
  private JCheckBox chkAutomaticComment;
  private JButton buttonSend;
  private JButton buttonMarkAsFixed;
  private MWPane textPage;

  /**
   * @param window Enclosing window.
   * @param pane Enclosing pane.
   * @param page Page.
   * @param errors List of errors.
   */
  CheckWikiContentPanel(
      CheckWikiWindow window, JTabbedPane pane,
      Page page, List<AlgorithmError> errors) {
    super(new GridBagLayout());
    this.window = window;
    this.pane = pane;
    this.page = page;
    this.errors = errors;
    setName(page.getTitle());
  }

  /**
   * @return Wiki.
   */
  @Override
  public EnumWikipedia getWiki() {
    if (window != null) {
      return window.getWikipedia();
    }
    return null;
  }

  /**
   * Initialize window.
   */
  void initialize() {

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

    textPage = new MWPane(getWiki(), page, window);

    // Comment
    JPanel panelComment = new JPanel(new GridBagLayout());
    GridBagConstraints constraints2 = new GridBagConstraints();
    constraints2.fill = GridBagConstraints.HORIZONTAL;
    constraints2.gridheight = 1;
    constraints2.gridwidth = 1;
    constraints2.gridx = 0;
    constraints2.gridy = 0;
    constraints2.insets = new Insets(0, 0, 0, 0);
    constraints2.ipadx = 0;
    constraints2.ipady = 0;
    constraints2.weightx = 0;
    constraints2.weighty = 0;
    chkAutomaticComment = window.createChkAutomaticComment(true, this);
    panelComment.add(chkAutomaticComment, constraints2);
    constraints2.gridx++;
    textComment = new JTextField(getComment(null));
    textComment.setEditable(false);
    constraints2.weightx = 1;
    panelComment.add(textComment, constraints2);
    constraints2.gridx++;
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.weightx = 1;
    constraints.weighty = 0;
    add(panelComment, constraints);
    constraints.gridy++;

    // Buttons
    JToolBar toolbarButtons = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarButtons.setFloatable(false);
    ActionOccurrence.addButton(
        toolbarButtons, textPage, ActionOccurrence.Occurrence.FIRST, true, true);
    ActionOccurrence.addButton(
        toolbarButtons, textPage, ActionOccurrence.Occurrence.PREVIOUS, true, true);
    ActionOccurrence.addButton(
        toolbarButtons, textPage, ActionOccurrence.Occurrence.NEXT, true, true);
    ActionOccurrence.addButton(
        toolbarButtons, textPage, ActionOccurrence.Occurrence.LAST, true, true);
    toolbarButtons.addSeparator();
    JButton buttonToc = window.createButtonToc(this, true);
    toolbarButtons.add(buttonToc);
    ActionInsertPredefinedText.addButton(
        toolbarButtons, textPage, this, null, true);
    JButton buttonValidate = window.createButtonValidate(this, true);
    toolbarButtons.add(buttonValidate);
    buttonSend = window.createButtonSend(this, true);
    buttonSend.setEnabled(false);
    toolbarButtons.add(buttonSend);
    if ((getWiki().getConnection().getUser() != null) &&
        (getWiki().getConnection().getUser().hasRight(User.RIGHT_DELETE))) {
      ActionDeletePage.addButton(
          toolbarButtons, window.getParentComponent(), this, this, true);
    }
    buttonMarkAsFixed = Utilities.createJButton(
        "gnome-dialog-apply.png", EnumImageSize.NORMAL,
        GT._T("Mark as already fixed"), false, null); // Mark as fixed
    buttonMarkAsFixed.setEnabled(true);
    buttonMarkAsFixed.setActionCommand(ACTION_MARK_AS_FIXED);
    buttonMarkAsFixed.addActionListener(this);
    toolbarButtons.add(buttonMarkAsFixed);
    ActionCheckArticle.addButton(
        window.getParentComponent(), toolbarButtons,
        getWiki(), page.getTitle(), textPage, true);
    ActionLinter.addButton(
        window, toolbarButtons,
        getWiki(), page.getTitle(), textPage, true);
    ActionDeadLink.addButton(
        window, toolbarButtons,
        getWiki(), page.getTitle(), textPage, true);
    toolbarButtons.addSeparator();
    if (Utilities.isDesktopSupported()) { // External Viewer
      ActionExternalViewer.addButton(
          toolbarButtons, getWiki(), page.getTitle(), false, true, true);
      ActionExternalViewer.addButton(
          toolbarButtons, getWiki(), page.getTitle(), ActionExternalViewer.ACTION_HISTORY, true, true);
      toolbarButtons.addSeparator();
    }
    ActionFullAnalysis.addButton(
        toolbarButtons, getWiki(), page.getTitle(), true, false, false);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.weightx = 1;
    constraints.weighty = 0;
    add(toolbarButtons, constraints);
    constraints.gridy++;

    // Errors list
    modelErrors = new DefaultListModel<CheckErrorPage>();
    listErrors = new JList<CheckErrorPage>(modelErrors);
    CheckErrorPageListCellRenderer cellRenderer = new CheckErrorPageListCellRenderer(false);
    cellRenderer.showCountOccurence(true);
    listErrors.setCellRenderer(cellRenderer);
    listErrors.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    listErrors.addListSelectionListener(new ListSelectionListener() {

      @Override
      public void valueChanged(ListSelectionEvent e) {
        if (e.getValueIsAdjusting()) {
          return;
        }
        actionSelectError();
      }
      
    });
    JScrollPane scrollErrors = new JScrollPane(listErrors);
    scrollErrors.setMinimumSize(new Dimension(200, 200));
    scrollErrors.setPreferredSize(new Dimension(200, 300));
    scrollErrors.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 1;
    add(scrollErrors, constraints);
    constraints.gridx++;

    // Page contents
    listErrors.addMouseListener(
        new CheckErrorPageListPopupListener(getWiki(), textPage, buttonValidate));
    textPage.setEditable(true);
    textPage.addPropertyChangeListener(
        MWPane.PROPERTY_MODIFIED,
        EventHandler.create(PropertyChangeListener.class, this, "updateComponentState"));
    textPage.setPopupListener(new MWPaneCheckWikiPopupListener(
        getWiki(), window));
    JComponent scrollContents = MWPane.createComplexPane(textPage);
    scrollContents.setMinimumSize(new Dimension(100, 100));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 1;
    constraints.weighty = 1;
    //panel.add(scrollPage, constraints);
    add(scrollContents, constraints);
    constraints.gridy++;
  }

  /**
   * Update component state.
   */
  public void updateComponentState() {
    if (buttonSend != null) {
      buttonSend.setEnabled((textPage != null) && (textPage.isModified()));
    }
    if (buttonMarkAsFixed != null) {
      buttonMarkAsFixed.setEnabled((errors != null) && (errors.size() == 1));
    }
  }

  /**
   * Action called when a page is selected (after page is loaded).
   * 
   * @param messages List to complete with messages (deleted pages, fixed pages...).
   */
  void actionPageSelected(List<String> messages) {
    if (page == null) {
      pane.remove(this);
      return;
    }

    // Deleted page
    if (Boolean.FALSE.equals(page.isExisting())) {
      String message = GT._T("The page {0} doesn''t exist on Wikipedia", page.getTitle());
      if (messages != null) {
        messages.add(message);
      } else {
        window.displayWarning(message);
      }
      if (errors != null) {
        for (AlgorithmError error : errors) {
          error.remove(page);
        }
      }
      pane.remove(this);
      if (errors != null) {
        for (AlgorithmError error : errors) {
          OnePageWindow.markPageAsFixed(error.getAlgorithm().getErrorNumberString(), page);
        }
      }
      window.actionSelectErrorType();
      return;
    }

    // Analyze initial errors
    textPage.setText(page.getContents());
    textPage.setModified(false);
    PageAnalysis pageAnalysis = page.getAnalysis(textPage.getText(), true);
    List<CheckErrorPage> errorsFound = AlgorithmError.analyzeErrors(
        window.allAlgorithms, pageAnalysis, false);
    modelErrors.clear();
    initialErrors = new ArrayList<CheckErrorPage>();
    boolean errorFound = false;
    int errorCount = 0;
    if (errorsFound != null) {
      for (CheckErrorPage tmpError : errorsFound) {
        modelErrors.addElement(tmpError);
        initialErrors.add(tmpError);
        errorCount++;
        for (AlgorithmError error : errors) {
          CheckErrorAlgorithm algorithm = error.getAlgorithm();
          if ((algorithm != null) &&
              (algorithm.getErrorNumber()== tmpError.getAlgorithm().getErrorNumber())) {
            errorFound = true;
          }
        }
      }
    }

    // Further analysis if expected error is not found
    if ((errors != null) &&
        !errors.isEmpty() &&
        (errorFound == false)) {
      int answer = JOptionPane.NO_OPTION;

      // Ask Check Wiki what errors are still detected
      List<CheckWikiDetection> detections = null;
      boolean shouldCheck = false;
      for (AlgorithmError error : errors) {
        if (error.getErrorNumber() < CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) {
          shouldCheck = true;
        }
      }
      if (shouldCheck) {
        CheckWiki checkWiki = APIFactory.getCheckWiki();
        detections = checkWiki.check(page);
      }

      // Check if the expected error is still detected
      boolean errorDetected = false;
      if (detections != null) {
        for (CheckWikiDetection detection : detections) {
          for (AlgorithmError error : errors) {
            if (detection.getErrorNumber() == error.getErrorNumber()) {
              errorDetected = true;
            }
          }
        }
      }

      // Construct a list of errors
      StringBuilder txtErrors = new StringBuilder();
      for (AlgorithmError error : errors) {
        if (txtErrors.length() > 0) {
          txtErrors.append("-");
        }
        txtErrors.append(error.getErrorNumber());
      }

      // Inform user
      if (!shouldCheck) {
        answer = JOptionPane.NO_OPTION;
      } else if (detections == null) {

        // Ask user if no information from Check Wiki
        answer = JOptionPane.YES_OPTION;
        if (window.yesAll) {
          answer = Utilities.YES_ALL_OPTION;
        } else if (window.noAll) {
          answer = Utilities.NO_ALL_OPTION;
        } else {
          Configuration config = Configuration.getConfiguration();
          if (!config.getBoolean(
              null,
              ConfigurationValueBoolean.CHECK_MARK_AS_FIXED)) {
            answer = window.displayYesNoAllWarning(GT._T(
                "The error n°{0} hasn''t been found on the page {1}.\n" +
                "Do you want to mark it as fixed?",
                new Object[] { txtErrors, page.getTitle() }));
          }
        }
      } else if (errorDetected) {

        // Inform user if Check Wiki still detects the error
        DetectionPanel panel = new DetectionPanel(detections, null);
        panel.setMessage(GT._T(
            "The error n°{0} hasn''t been detected in page {1}, but CheckWiki still reports it.",
            new Object[] { txtErrors, page.getTitle() }));
        JOptionPane.showMessageDialog(
            window.getParentComponent(), panel,
            Version.PROGRAM, JOptionPane.WARNING_MESSAGE);
      } else {

        // Inform user if Check Wiki doesn't detect the error anymore
        String message = GT._T(
            "The error n°{0} has already been fixed in page {1}.",
            new Object[] { txtErrors, page.getTitle() });
        if (messages != null) {
          messages.add(message);
        } else {
          window.displayWarning(message);
        }
        answer = JOptionPane.YES_OPTION;
      }

      // Act according to user answer
      switch (answer) {
      case Utilities.YES_ALL_OPTION:
        window.yesAll = true;
        answer = JOptionPane.YES_OPTION;
        break;

      case Utilities.NO_ALL_OPTION:
        window.noAll = true;
        answer = JOptionPane.NO_OPTION;
        break;
      }
      switch (answer) {
      case JOptionPane.YES_OPTION:
        if (errorCount == 0) {
          pane.remove(this);
        }
        for (AlgorithmError error : errors) {
          error.remove(page);
        }
        for (AlgorithmError error : errors) {
          OnePageWindow.markPageAsFixed(
              error.getAlgorithm().getErrorNumberString(), page);
        }
        window.actionSelectErrorType();
        if (errorCount == 0) {
          return;
        }
        break;
      }
    }

    // Select error
    int index = modelErrors.indexOf(window.listAllErrors.getSelectedItem());
    if (index >= 0) {
      listErrors.setSelectedIndex(index);
    } else if (modelErrors.getSize() > 0) {
      listErrors.setSelectedIndex(0);
    }

    // Automatic fix of some errors
    if ((initialErrors != null) && (textPage != null)) {
      String initialContents = textPage.getText();
      Collection<CheckErrorAlgorithm> algorithms = new ArrayList<CheckErrorAlgorithm>();
      for (CheckErrorPage initialError : initialErrors) {
        algorithms.add(initialError.getAlgorithm());
      }
      String contents = AutomaticFormatter.tidyArticle(
          page, initialContents, algorithms, false, null);
      if (!contents.equals(initialContents)) {
        textPage.changeText(contents);
        actionValidate();
      }
    }
  }

  /**
   * @return Current selected error.
   */
  public CheckErrorPage getSelectedError() {
    Object selection = listErrors.getSelectedValue();
    if (selection instanceof CheckErrorPage) {
      return (CheckErrorPage) selection;
    }
    return null;
  }

  /**
   * Action called when an error is selected. 
   */
  void actionSelectError() {
    CheckErrorPage errorSelected = getSelectedError();
    if (errorSelected == null) {
      textPage.setFormatter(new MWPaneBasicFormatter());
    } else {
      CheckErrorAlgorithm algorithm = errorSelected.getAlgorithm();
      MWPaneFormatter formatter = textPage.getFormatter();
      if (formatter instanceof MWPaneCheckWikiFormatter) {
        MWPaneCheckWikiFormatter cwFormatter =
          (MWPaneCheckWikiFormatter) formatter;
        if (!cwFormatter.isSameAlgorithm(algorithm)) {
          formatter = new MWPaneCheckWikiFormatter(algorithm);
          textPage.setFormatter(formatter);
        } else {
          textPage.resetAttributes();
        }
      } else {
        formatter = new MWPaneCheckWikiFormatter(algorithm);
        textPage.setFormatter(formatter);
      }
    }
    listErrors.repaint();
    updateComponentState();
    window.displayErrorDescription();
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    if (OnePageWindow.ACTION_FULL_ANALYSIS_PAGE.equals(e.getActionCommand())) {
      Controller.runFullAnalysis(page.getTitle(), null, getWiki());
    } else if (ACTION_MARK_AS_FIXED.equals(e.getActionCommand())) {
      actionMarkAsFixed();
    } else if (OnePageWindow.ACTION_SEND.equals(e.getActionCommand())) {
      actionSend();
    } else if (OnePageWindow.ACTION_TOC.equals(e.getActionCommand())) {
      actionToc();
    } else if (OnePageWindow.ACTION_VALIDATE.equals(e.getActionCommand())) {
      actionValidate();
    }
  }

  /**
   * Mark a page as fixed. 
   */
  private void actionMarkAsFixed() {

    // Unique algorithm
    CheckErrorAlgorithm algorithm = null;
    AlgorithmError error = null;
    if ((errors != null) && (errors.size() == 1)) {
      error = errors.get(0);
      algorithm = error.getAlgorithm();
    }
    if ((error == null) || (algorithm == null)) {
      return;
    }

    // Ask for confirmation
    if (window.displayYesNoWarning(GT._T(
        "Do you want to mark {0} as fixed for error n°{1}?",
        new Object[] { page.getTitle(), algorithm.getErrorNumberString() })) != JOptionPane.YES_OPTION) {
      return;
    }

    // Check if error is still present
    PageAnalysis pageAnalysis = page.getAnalysis(textPage.getText(), true);
    CheckErrorPage errorPage = AlgorithmError.analyzeError(
        algorithm, pageAnalysis);
    if ((errorPage.getResults() != null) &&
        (!errorPage.getResults().isEmpty())) {
      String message =
          GT.__(
              "The error n°{0} is still found {1} time on the page.",
              "The error n°{0} is still found {1} times on the page.",
              errorPage.getResults().size(),
              new Object[] { algorithm.getErrorNumberString(), errorPage.getResults().size() }) +
          "\n" +
          GT._T("Are you really sure that you want to mark it as fixed ?");
      if (window.displayYesNoWarning(message) != JOptionPane.YES_OPTION) {
        return;
      }
    } else if (errorPage.getErrorFound()) {
      if (window.displayYesNoWarning(GT._T(
          "The error n°{0} is still found on the page.\n" +
          "Are you really sure that you want to mark it as fixed ?",
          algorithm.getErrorNumberString())) != JOptionPane.YES_OPTION) {
        return;
      }
    } else {
      // Check if error was initially present
      for (int i = 0; i < modelErrors.size(); i++) {
        if (modelErrors.elementAt(i) != null) {
          CheckErrorPage tmp = modelErrors.elementAt(i);
          if (tmp.getAlgorithm() == algorithm) {
            window.displayWarning(GT._T(
                "You have already fixed this error by modifying the page.\n" +
                "You should send your modifications, the page will be marked as fixed."));
            return;
          }
        }
      }
    }

    // Mark as fixed
    error.remove(page);
    if (!textPage.isModified()) {
      pane.remove(this);
    }
    if (error.getPageCount() == 0) {
      Configuration configuration = Configuration.getConfiguration();
      if (!configuration.getBoolean(
          null,
          ConfigurationValueBoolean.CHECK_SHOW_0_ERRORS)) {
        window.listAllErrors.removeItem(error);
      }
    }
    window.actionSelectErrorType();
    OnePageWindow.markPageAsFixed(error.getAlgorithm().getErrorNumberString(), page);
  }

  /**
   * Compute comment.
   * 
   * @param errorsFixed Errors fixed
   * @return Comment.
   */
  private String getComment(List<AlgorithmError.Progress> errorsFixed) {
    String comment = getWiki().getCWConfiguration().getComment(errorsFixed);
    if ((comment != null) && (comment.length() > 0)) {
      Configuration config = Configuration.getConfiguration();
      String userComment = config.getString(getWiki(), ConfigurationValueString.COMMENT);
      if ((userComment == null) || (userComment.trim().length() == 0)) {
        userComment = config.getString(null, ConfigurationValueString.COMMENT);
      }
      if ((userComment != null) && (userComment.trim().length() > 0)) {
        comment = userComment.trim() + " - " + comment;
      }
    }
    return comment;
  }

  /**
   * @return Errors fixed.
   */
  private List<AlgorithmError.Progress> computeErrorsFixed() {
    return AlgorithmError.computeErrorsFixed(initialErrors, textPage.getText(), false);
  }

  /**
   * Send page.
   */
  private void actionSend() {
    // Check page text to see what errors are still present
    final List<AlgorithmError.Progress> errorsFixed = computeErrorsFixed();
    updateComment(errorsFixed);

    // Check that a comment is available
    if (textComment.getText().trim().length() == 0) {
      Utilities.displayWarning(getParent(), GT._T(
          "A comment is required for sending the page."));
      return;
    }

    // Count contributions
    Contributions contributions = new Contributions(getWiki());
    contributions.increasePages(1);
    for (AlgorithmError.Progress errorFixed : errorsFixed) {
      CheckErrorAlgorithm algorithm = errorFixed.algorithm;
      contributions.increaseCheckWikiError(algorithm.getErrorNumber(), 1);
    }

    // Check for errors fixed
    boolean updateISBNWarning = false;
    boolean createISBNWarning = false;
    boolean updateISSNWarning = false;
    boolean createISSNWarning = false;
    boolean updateDuplicateArgsWarning = false;
    boolean createDuplicateArgsWarning = false;
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

    // Send page
    final Configuration configuration = Configuration.getConfiguration();
    SendWorker sendWorker = new SendWorker.Builder().
        allowISBNWarning(updateISBNWarning, createISBNWarning).
        allowISSNWarning(updateISSNWarning, createISSNWarning).
        allowDuplicateArgsWarning(updateDuplicateArgsWarning, createDuplicateArgsWarning).
        createWorker(
          getWiki(), window,
          page, textPage.getText(), textComment.getText(),
          true, // TODO: BOT
          configuration.getBoolean(
              null,
              ConfigurationValueBoolean.MARK_EDIT_MINOR),
          configuration.getBoolean(
              null,
              ConfigurationValueBoolean.FORCE_WATCH),
          contributions, errorsFixed);
    sendWorker.setListener(new DefaultBasicWorkerListener() {
      @Override
      public void afterFinished(
          @SuppressWarnings("unused") BasicWorker worker,
          boolean ok) {
        afterSendingFinished(errorsFixed, ok);
      }
    });
    sendWorker.start();
  }

  /**
   * @param errorsFixed
   * @param ok
   */
  void afterSendingFinished(List<AlgorithmError.Progress> errorsFixed, boolean ok) {
    if (ok) {
      // Close pane
      pane.remove(this);

      // Remove errors fixed
      List<AlgorithmError> errorsToBeRemoved = new ArrayList<AlgorithmError>();
      for (AlgorithmError.Progress errorFixed : errorsFixed) {
        CheckErrorAlgorithm algoFixed = errorFixed.algorithm;
        for (int posError = 0; posError < window.modelAllErrors.getSize(); posError++) {
          Object element = window.modelAllErrors.getElementAt(posError);
          if (element instanceof AlgorithmError) {
            final AlgorithmError tmpError = (AlgorithmError) element;
            if (tmpError.getAlgorithm().getErrorNumberString().equals(algoFixed.getErrorNumberString()) &&
                errorFixed.full) {
              tmpError.remove(page);
              if (tmpError.getPageCount() == 0) {
                errorsToBeRemoved.add(tmpError);
              }
            }
          }
        }
      }
      final Configuration configuration = Configuration.getConfiguration();
      if (!configuration.getBoolean(
          null,
          ConfigurationValueBoolean.CHECK_SHOW_0_ERRORS)) {
        for (AlgorithmError tmpError : errorsToBeRemoved) {
          window.listAllErrors.removeItem(tmpError);
        }
      }
      window.actionSelectErrorType();
    }
  }

  /**
   * Display table of contents.
   */
  private void actionToc() {
    textPage.toggleToc();
  }

  /**
   * Validate current text and recompute errors.
   */
  private void actionValidate() {
    // Check for new errors
    PageAnalysis pageAnalysis = page.getAnalysis(textPage.getText(), true);
    List<CheckErrorPage> errorsFound = AlgorithmError.analyzeErrors(
        window.allAlgorithms, pageAnalysis, false);
    if (errorsFound != null) {
      for (CheckErrorPage tmpError : errorsFound) {
        boolean errorFound = false;
        for (int index = 0; index < modelErrors.getSize(); index++) {
          CheckErrorPage errorModel = modelErrors.get(index);
          if ((errorModel != null) &&
              (errorModel.getAlgorithm() != null) &&
              (errorModel.getAlgorithm().equals(tmpError.getAlgorithm()))) {
            errorFound = true;
            modelErrors.set(index, tmpError);
          }
        }
        if (!errorFound) {
          modelErrors.addElement(tmpError);
        }
      }
    }
    for (int index = 0; index < modelErrors.getSize(); index++) {
      CheckErrorPage errorModel = modelErrors.get(index);
      if ((errorsFound == null) || (!errorsFound.contains(errorModel))) {
        CheckErrorPage newError = new CheckErrorPage(page, errorModel.getAlgorithm());
        modelErrors.set(index, newError);
      }
    }

    actionSelectError();
    updateComment(null);
    Object selected = listErrors.getSelectedValue();
    if (selected instanceof CheckErrorPage) {
      CheckErrorPage errorPage = (CheckErrorPage) selected;
      if (!errorPage.getErrorFound()) {
        int index = listErrors.getSelectedIndex();
        if (index < modelErrors.getSize() - 1) {
          listErrors.setSelectedIndex(index + 1);
        }
      }
    }
  }

  /**
   * Update automatic comment.
   * 
   * @param errorsFixed Errors.
   */
  private void updateComment(List<AlgorithmError.Progress> errorsFixed) {
    if ((chkAutomaticComment != null) &&
        (textComment != null)) {
      textComment.setEditable(!chkAutomaticComment.isSelected());
      if (chkAutomaticComment.isSelected()) {
        textComment.setText(getComment((errorsFixed != null) ? errorsFixed : computeErrorsFixed()));
      }
    }
  }

  /* (non-Javadoc)
   * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
   */
  @Override
  public void itemStateChanged(ItemEvent e) {
    if ((e == null) || (e.getSource() == null)) {
      return;
    }
    Object source = e.getSource();
    if ((source == chkAutomaticComment)) {
      updateComment(null);
    }
  }

  /**
   * @return Page.
   * @see org.wikipediacleaner.api.dataaccess.PageProvider#getPage()
   */
  @Override
  public Page getPage() {
    return page;
  }

  // ===========================================================================
  // Implementation of ListenerPageDeletion
  // ===========================================================================

  /**
   * Notification of the deletion of a page.
   * 
   * @param pageName Name of the page.
   */
  @Override
  public void pageDeleted(String pageName) {
    pane.remove(this);
  }
}