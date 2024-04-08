(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)

// This demo requires Python4Delphi: https://github.com/Embarcadero/python4delphi
// This demo requires PyrthonEnvironment: https://github.com/Embarcadero/PythonEnvironments

unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, PythonEngine,
  FMX.PythonGUIInputOutput, PyCommon, PyModule, PyPackage, Transformers,
  PyEnvironment, PyEnvironment.Embeddable, FMX.StdCtrls, TensorFlow, PyTorch,
  FMX.Edit, FMX.Layouts;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    memLog: TMemo;
    PyEmbeddedEnvironment1: TPyEmbeddedEnvironment;
    PyTorch1: TPyTorch;
    Transformers1: TTransformers;
    loPrompt: TLayout;
    edtPrompt: TEdit;
    btnAuto: TButton;
    btnPipeline: TButton;
    memOutput: TMemo;
    AniIndicator1: TAniIndicator;
    procedure btnAutoClick(Sender: TObject);
    procedure btnPipelineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1Error(Sender: TObject;
      const AException: Exception);
    procedure PyEmbeddedEnvironment1Ready(Sender: TObject;
      const APythonVersion: string);
    procedure PyTorch1BeforeInstall(Sender: TObject);
    procedure Transformers1BeforeInstall(Sender: TObject);
  private
    procedure BeginSetup;
    procedure EndSetup;

    function GenerateAuto(const APrompt: string): string;
    function GeneratePipeline(const APrompt: string): string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  VarPyth, PyUtils;

  {
  ||||||||| IMPORTANT |||||||||
  If you notice a dll import error, you may install the Microsoft Visual C++ Redistributable.
  Microsoft Visual C++ Redistributable is not installed, this may lead to the DLL load failure.
  -> It can be downloaded at https://aka.ms/vs/16/release/vc_redist.x64.exe
  }

procedure TForm1.FormCreate(Sender: TObject);
begin
  var LSetup := PyEmbeddedEnvironment1.SetupAsync();
  var LActivate := PyEmbeddedEnvironment1.ActivateAsync(LSetup);
end;

procedure TForm1.BeginSetup;
begin
  AniIndicator1.Visible := true;
  AniIndicator1.Enabled := true;
  btnAuto.Enabled := false;
  btnPipeline.Enabled := false;
end;

procedure TForm1.EndSetup;
begin
  AniIndicator1.Visible := false;
  AniIndicator1.Enabled := false;
  btnAuto.Enabled := true;
  btnPipeline.Enabled := true;
end;

procedure TForm1.btnAutoClick(Sender: TObject);
begin
  memOutput.Text := GenerateAuto(edtPrompt.Text);
end;

procedure TForm1.btnPipelineClick(Sender: TObject);
begin
  memOutput.Text := GeneratePipeline(edtPrompt.Text);
end;

function TForm1.GenerateAuto(const APrompt: string): string;
begin
  // Import the tokenizer and model modules
  var LAutoTokenizer := Transformers1.transformers.AutoTokenizer;
  var LAutoModelForCausalLM := Transformers1.transformers.AutoModelForCausalLM;

  // Load the tokenizer
  {
  Model reference: https://huggingface.co/openai-community/gpt2
  }
  BuiltinModule.print('---------------------------<<GPT2 Fast Tokenizer>>---------------------------');
  var LTokenizer := LAutoTokenizer.from_pretrained('gpt2');
  BuiltinModule.print(LTokenizer);

  BuiltinModule.print('---------------------------<<GPT2 Model>>---------------------------');
  var LModel := LAutoModelForCausalLM.from_pretrained('gpt2');
  BuiltinModule.print(LModel);

  // Configuring the tokenizer
  LTokenizer.pad_token_id := LTokenizer.eos_token_id;

  BuiltinModule.print('---------------------------<<Inference>>---------------------------');

  // Tokenize the input
  var LEncodedInput := LTokenizer.__call__(APrompt, return_tensors := 'pt');

  // The generation step
  var LOutput := LModel.generate(
    input_ids := LEncodedInput.input_ids,
    attention_mask := LEncodedInput.attention_mask,

    //This parameter defines the maximum length of the sequence to be generated.
    //The model stops generating additional tokens when this length is reached.
    max_length := 50,

    //Bundle search is a technique used in NLP for text generation where the model considers several possible next words
    //at each step and keeps the most promising sequences (or "bundles") of tokens at each step.
    //Using 5 beams means the model tracks 5 potential sequences at each step, which can lead to higher quality results but requires more computational resources.
    num_beams := 5,

    // This setting prevents the model from repeating the same n-grams (in this case, sequences of 2 tokens) in the output text.
    //Helps reduce repetitiveness in the generated text.
    no_repeat_ngram_size := 2,

    // This parameter tells the model to stop generating text as soon as all beam candidates reach the end-of-sentence token.
    //It can make the generation process more efficient by stopping the search as soon as a satisfactory output is found.
    early_stopping := true,

    // Whether or not to return a ModelOutput instead of a plain tuple.
    return_dict_in_generate := true
  );

  // Get the model output tensor
  var LGeneratedTokenIds := LOutput.sequences[0];

  // Decode tokens to text
  var LDecodedOutput := LTokenizer.decode(LGeneratedTokenIds, skip_special_tokens := true);
  BuiltinModule.print(LDecodedOutput);

  Result := LDecodedOutput;
end;

function TForm1.GeneratePipeline(const APrompt: string): string;
begin
  // https://huggingface.co/docs/transformers/main_classes/pipelines

  BuiltinModule.print('---------------------------<<Inference>>---------------------------');

  var LPipeline := Transformers1.transformers.pipeline;

  // Helper function for reproducible behavior to set the seed in random, numpy, torch and/or tf (if installed).
  Transformers1.transformers.set_seed(42);

  var LGenerator := LPipeline.__call__('text-generation', model := 'gpt2');
  var LOutput := LGenerator.__call__(APrompt, max_length := 50, num_return_sequences := 3);

  if VarIsNull(LOutput) then
    Exit('Operation failed');

  for var LSequence in VarPyIterate(LOutput) do
    Result := Result + LSequence.GetItem('generated_text') + sLineBreak;
end;

procedure TForm1.PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  BeginSetup();
  memLog.Lines.Add('Setting up...');
end;

procedure TForm1.PyEmbeddedEnvironment1Error(Sender: TObject;
  const AException: Exception);
begin
  AniIndicator1.Visible := false;
  AniIndicator1.Enabled := false;
  Application.ShowException(AException);
  memLog.Lines.Add('Operation failed.');
end;

procedure TForm1.PyEmbeddedEnvironment1Ready(Sender: TObject;
  const APythonVersion: string);
begin
  EndSetup();
  memLog.Lines.Add('All done!');
end;

procedure TForm1.PyTorch1BeforeInstall(Sender: TObject);
begin
  memLog.Lines.Add('Installing Torch (it might take a while).');
end;

procedure TForm1.Transformers1BeforeInstall(Sender: TObject);
begin
  memLog.Lines.Add('Installing Transformers (it might take a while).');
end;

end.
