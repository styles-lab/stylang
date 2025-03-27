/// The main entry of one stylang app.
fn main() -> view {
    let stat err_msg = none;

    <navigation-stack>
        <center>
            <column>
                if solidity.is_connected() {
                    <label class.theme="header" text="Sponsor styles-lab"/>
                } else {
                    <label class.theme="header" text="Connect to Etherwallet to start donating"/>
                }
                
                <row>
                    <text-field id="value" prompt="Donate via ethereum network with a minimum donation of 0.1eth."/>
                    <button 
                        icon="assets://ethereum.svg" 
                        text="Donate" 
                        on-click.throttled(10) = || donate(value);
                        /// status value bind syntax.
                        on-click.error = $err_msg
                    />
                </row>
            </column>
        </center>
    </navigation-stack>
}

/// extern fn declaration.
extern fn donate(num: bignum) -> [u8;20] ?? data; 