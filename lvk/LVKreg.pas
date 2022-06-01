{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit registers all the components into the IDE.
}
unit LVKreg;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 27 $
// $Archive: /Components/LVK/source/LVKreg.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

procedure Register;

{$R lvkWizard.dcr}
{$R lvkBinaryCompare.dcr}
{$R lvkBinaryCompare2.dcr}
{$R lvkConsole.dcr}
{$R lvkDataSettoExcel.dcr}
{$R lvkErrorDialog.dcr}
{$R lvkExceptionMapper.dcr}
{$R lvkFormBinaryFile.dcr}
{$R lvkHideStrings.dcr}
{$R lvkLDAP.dcr}
{$R lvkMailslots.dcr}
{$R lvkActiveScriptComponent.dcr}
{$R lvkPaintBox.dcr}
{$R lvkRegExp.dcr}
{$R lvkStatusBar.dcr}
{$R lvkThread.dcr}
{$R lvkThreadButton.dcr}
{$R lvkVersionInformation.dcr}
{$R lvkFormVersionInformation.dcr}
{$R lvk3DGradientLabel.dcr}
{$R lvkEdits.dcr}
{$R lvkFileEdits.dcr}
{$R lvkEnabler.dcr}
{$R lvkXP.dcr}
{$R lvkState.dcr}
{$R lvkTrayIcon.dcr}
{$R lvkGlobalHotKeys.dcr}
{$R lvkValidationLabel.dcr}
{$R lvkValidators.dcr}
{$R lvkLabel.dcr}
{$R lvkCheckBox.dcr}
{$R lvkDropFiles.dcr}
{$R lvkStringList.dcr}
{$R lvkRadioButton.dcr}
{$R lvkProgressBar.dcr}
{$R lvkDoubleBuffered.dcr}
{$R lvkParameters.dcr}
{$R lvkGroupBox.dcr}
{$R lvkThreadInfoDispatcher.dcr}
{$R lvkDirectoryDialog.dcr}
{$R lvkEventSystem.dcr}

implementation

uses
  SysUtils, Classes,
  lvkRegExp, lvkPaintBox, lvkMailslots, lvkWizard, lvkVersionInformation,
  lvkErrorDialog, lvkConsole, lvkDataSetToExcel, lvkFormBinaryFile,
  lvkHideStrings, lvkStatusBar, lvkBinaryCompare, lvkBinaryCompare2,
  lvkExceptionMapper, lvkThread, lvkThreadPool, lvkThreadCommunicationsQueue,
  lvkThreadCommunicationsStream, lvkSyncObjs, lvkThreadButton,
  lvkActiveScriptComponent, lvkTCPIPCommunicationsQueue,
  lvk3DGradientLabel, lvkLDAP, lvkEdits, lvkEnabler,
  lvkFileEdits, lvkXP, lvkSpeedEdit, lvkMaskEdit, lvkDropDownEdit,
  lvkMemo, lvkComboBox, lvkListBox, lvkSizeGrip, lvkState,
  lvkINIFileStateStorage, lvkFormState, lvkComponentsState,
  lvkRegistryStateStorage, lvkTrayIcon, lvkGlobalHotKeys, lvkValidationLabel,
  lvkValidators, lvkValidatorEnabler, lvkLabel, lvkCheckBox, lvkDropFiles,
  lvkStringList, lvkRadioButton, lvkNumEdits, lvkMultiSpeedEdit,
  lvkNumSpeedEdits, lvkProgressBar, lvkValidationImage, lvkDropTarget,
  lvkDoubleBuffered, lvkGroupBox, lvkParameters, lvkThreadInfoDispatcher,
  lvkFormVersionInformation, lvkPanelDropDownEdit, lvkDirectoryDialog,
  lvkEventSystem;

const
  TAB_PREFIX        = 'LVK ';
  VISUAL_TAB        = TAB_PREFIX + 'Visual';
  NON_VISUAL_TAB    = TAB_PREFIX + 'Non-visual';
  THREADS_TAB       = TAB_PREFIX + 'Threads';
  VALIDATORS_TAB    = TAB_PREFIX + 'Validators';
  DEPRECATED_TAB    = TAB_PREFIX + 'Deprecated';
  DIALOGS_TAB       = TAB_PREFIX + 'Dialogs';

procedure Register;
const
  LVK_Visual  : array[1..37] of TComponentClass = (
    TlvkPaintBox,
    TlvkWizard, TlvkWizardPanel,
    TlvkConsole,
    TlvkStatusBar, TlvkLabelStatusBarPanel,
      TlvkClockStatusBarPanel, TlvkKeyStatusBarPanel,
      TlvkProgressBarStatusBarPanel, TlvkEditStatusBarPanel,
      TlvkComboBoxStatusBarPanel, TlvkHintStatusBarPanel,
    Tlvk3DGradientLabel,
    TlvkEdit, TlvkListBox, TlvkComboBox, TlvkMemo, TlvkSpeedEdit,
      TlvkSpinEdit, TlvkDropDownEdit, TlvkMaskEdit, TlvkIntegerEdit,
      TlvkFloatEdit, TlvkIntegerSpeedEdit, TlvkFloatSpeedEdit,
      TlvkPanelDropDownEdit,
    TlvkDirectoryEdit, TlvkFileEdit,
    TlvkLabel,
    TlvkCheckBox, TlvkRadioButton,
    TlvkSpinButton,
    TlvkMultiSpeedEdit,
    TlvkProgressBar, TlvkSegmentedProgressBar,
    TlvkExpandableGroupBox, TlvkCheckBoxExpandableGroupBox);
  LVK_Deprecated : array[1..2] of TComponentClass = (
    TlvkPatchBuilder, TlvkPatchApplier
  );
  LVK_Dialogs : array[1..1] of TComponentClass = (
    TlvkDirectoryDialog
  );
  LVK_NonVisual  : array[1..36] of TComponentClass = (
    TlvkRegExp,
    TlvkMailslotClient, TlvkMailslotServer,
    TlvkErrorDialog,
    TlvkFormBinaryFileCollection, TlvkHideStrings,
    TlvkPatchBuilder2, TlvkPatchApplier2,
    TlvkLDAPConnection, TlvkLDAPQuery,
    TlvkVersionInformation, TlvkFormVersionInformation,
    TlvkCheckBoxEnabler, TlvkManualEnabler, TlvkThreadEnabler,
      TlvkEventBasedEnabler, TlvkValidatorEnabler,
    TlvkWTSSessionChange,
    TlvkDataSettoExcel,
    TlvkExceptionMapper,
    TlvkActiveScriptWrapper,
    TlvkINIFileStateStorage, TlvkFormState, TlvkComponentsState,
      TlvkRegistryStateStorage,
    TlvkTrayIcon, TlvkAnimatedTrayIcon,
    TlvkGlobalHotKeys,
    TlvkDropFiles, TlvkStandardDropTarget,
    TlvkStringList,
    TlvkDoubleBuffered,
    TlvkParameters,
    TlvkEventServer, TlvkEventClient, TlvkEventDispatcher);
  LVK_Validators : array[1..15] of TComponentClass = (
    TlvkValidationLabel, TlvkValidationImage,
    TlvkRequiredValidator, TlvkRegularExpressionValidator,
      TlvkIntegerValidator, TlvkRangedIntegerValidator,
      TlvkFloatValidator, TlvkAnyValidator,
    TlvkEmailAddressValidator, TlvkHTTPAddressValidator,
      TlvkFTPAddressValidator,
    TlvkUsernameValidator, TlvkPasswordValidator,
    TlvkLengthValidator,
    TlvkEventValidator
  );
  LVK_Threads   : array[1..17] of TComponentClass = (
    TlvkThreadWrapper, TlvkFuture, TlvkThreadPool,
    TlvkThreadCommunicationsQueue, TlvkThreadCommunicationsStream,
      TlvkThreadCommunicationsQueueProxy,
    TlvkThreadInfoDispatcher,
    TlvkCriticalSection, TlvkEvent, TlvkMutex, TlvkSemaphore,
      TlvkSingleWriterMultipleReaders,
    TlvkThreadButton, TlvkThreadBitBtn, TlvkThreadSpeedButton,
    TlvkTCPIPCommunicationsQueueClient, TlvkTCPIPCommunicationsQueueServer
  );
var
  Index : Integer;
begin
  for Index := Low(LVK_Visual) to High(LVK_Visual) do
    RegisterComponents(VISUAL_TAB, [LVK_Visual[Index]]);

  for Index := Low(LVK_NonVisual) to High(LVK_NonVisual) do
    RegisterComponents(NON_VISUAL_TAB, [LVK_NonVisual[Index]]);

  for Index := Low(LVK_Validators) to High(LVK_Validators) do
    RegisterComponents(VALIDATORS_TAB, [LVK_Validators[Index]]);

  for Index := Low(LVK_Threads) to High(LVK_Threads) do
    RegisterComponents(THREADS_TAB, [LVK_Threads[Index]]);

  for Index := Low(LVK_Dialogs) to High(LVK_Dialogs) do
    RegisterComponents(DIALOGS_TAB, [LVK_Dialogs[Index]]);

  for Index := Low(LVK_Deprecated) to High(LVK_Deprecated) do
    RegisterComponents(DEPRECATED_TAB, [LVK_Deprecated[Index]]);
end;

end.
