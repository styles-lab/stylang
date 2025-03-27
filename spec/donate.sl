/// The main entry of one stylang app.
fn main() -> view {
    let value: @state @option string = none;

    <navigation-stack>
        <center>
            <column>
                // core mod `web3`.
                if web3::is_connected() {
                    <label class.theme="header" text="Sponsor styles-lab"/>
                } else {
                    <label class.theme="header" text="Connect to Etherwallet to start donating"/>
                }
                
                <row>
                    <text-field text=value prompt="Donate via ethereum network with a minimum donation of 0.1eth."/>

                    <
                        button 
                        icon="assets://ethereum.svg" 
                        text="Donate" 
                        on-click.solidity={
                            || donate(/*implicit type conversion*/ value) 
                        }
                    />
                </row>
            </column>
        </center>
    </navigation-stack>
}

fn donate(amount: bignum) {
    use solidity::erc20;

    try {
        let tx = erc20.transfer({value: amount})?;

    } catch(err) {

    }
}