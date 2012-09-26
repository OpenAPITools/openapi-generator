require 'spec_helper'

describe String do

  it "underscores" do
    "thisIsATest".underscore.should == "this_is_a_test"
  end
  
  it "camelizes" do
    "camel_toe".camelize.should == "CamelToe"
  end

  it "camelizes with leading minisculity" do
    "dromedary_larry".camelize(:lower).should == "dromedaryLarry"
  end
  
end

describe Hash do
  
  it "symbolizes keys" do
    h = {'a' => 1, :b => 2 }
    h.symbolize_keys.should be_a Hash
    h.symbolize_keys.keys.should == [:a, :b]
  end

  it "symbolizes and underscores keys" do
    h = {'assHat' => 1, :bargainBasement => 2 }
    h.symbolize_and_underscore_keys.should be_a Hash
    h.symbolize_and_underscore_keys.keys.should == [:ass_hat, :bargain_basement]
  end

end